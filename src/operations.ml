open Xmlt

let cell_address_to_indexes s =
  let rec split chars nums i =
    if i >= String.length s then (chars, nums)
    else
      let c = String.get s i in
      if Char.code c >= Char.code 'A' && Char.code c <= Char.code 'Z' then
        split (chars ^ String.make 1 c) nums (i + 1)
      else split chars (nums ^ String.make 1 c) (i + 1)
  in
  let letters, digits = split "" "" 0 in
  let column_number =
    String.fold_left
      (fun acc c -> (acc * 26) + (Char.code c - Char.code 'A' + 1))
      0 letters
  in
  let row_number = int_of_string digits in
  (row_number - 1, column_number - 1)

let get_xml str =
  let el ((_, tag), atts) xs =
    let atts =
      List.fold_left
        (fun map ((_, name), v) -> Nmap.add name v map)
        Nmap.empty atts
    in
    `Node (tag, atts, xs)
  in
  let data d = `Data d in
  Xmlm.input_doc_tree ~el ~data str |> snd

let utf_decode ~enc s =
  let b = Buffer.create (String.length s) in
  let decoder = Uutf.decoder ~encoding:enc (`String s) in
  let rec loop () =
    match Uutf.decode decoder with
    | `Uchar u ->
        Buffer.add_utf_8_uchar b u;
        loop ()
    | `End -> Buffer.contents b
    | `Malformed _ -> failwith "Malformed UTF-8 sequence"
    | `Await -> assert false
  in
  loop ()

let rec accumulate_shared_strings ~enc acc = function
  | `Data d -> (d |> utf_decode ~enc) :: acc
  | `Node (_, _, xs) -> List.fold_left (accumulate_shared_strings ~enc) acc xs

let place_shared_strings ss_lst array (i, j) = function
  | [ `Node ("v", _, [ `Data value ]) ] ->
      let str =
        List.nth ss_lst (value |> int_of_string) |> Format.sprintf "\"%s\""
      in
      array.(i).(j) <- str
  | _ -> ()

let rec xml_to_csv ~ignore_hiddens ss_lst array (i, j) = function
  | `Data value -> array.(i).(j) <- value
  | `Node ("f", _, _) -> () (* ignore function definitions *)
  | `Node ("c", atts, xs) -> begin
      let i, j =
        match Nmap.find_opt "r" atts with
        | Some ca -> cell_address_to_indexes ca
        | _ -> assert false
      in
      match Nmap.find_opt "t" atts with
      | Some "s" -> place_shared_strings ss_lst array (i, j) xs
      | _ -> List.iter (xml_to_csv ~ignore_hiddens ss_lst array (i, j)) xs
    end
  | `Node ("row", atts, xs) -> begin
      match Nmap.find_opt "hidden" atts with
      | Some "1" when ignore_hiddens -> ()
      | _ -> List.iter (xml_to_csv ~ignore_hiddens ss_lst array (i, j)) xs
    end
  | `Node (_, _, xs) ->
      List.iter (xml_to_csv ~ignore_hiddens ss_lst array (i, j)) xs

let get_dim xml =
  let rec calculate_dim ((k, l) as acc) = function
    | `Node ("c", atts, _) -> begin
        match Nmap.find_opt "r" atts with
        | Some d ->
            let i, j = cell_address_to_indexes d in
            (max i k, max j l)
        | None -> assert false
      end
    | `Node (_, _, xs) -> List.fold_left calculate_dim acc xs
    | `Data _ -> acc
  in
  match xml with
  | `Node ("worksheet", _, xs) -> begin
      match
        List.find_opt
          (function `Node ("dimension", _, []) -> true | _ -> false)
          xs
      with
      | Some (`Node ("dimension", atts, [])) -> begin
          match Nmap.find_opt "ref" atts with
          | Some d -> begin
              d |> String.split_on_char ':' |> function
              | [ _; l ] -> Ok (l |> cell_address_to_indexes)
              | _ -> Error (`Msg "Incorrect dimension")
            end
          | None -> Error (`Msg "No ref attribute inside dimension")
        end
      | _ -> Ok (List.fold_left calculate_dim (0, 0) xs)
    end
  | _ -> Error (`Msg "No worksheet node")

let get_shared_strings ~enc ic =
  let xmlm_enc = Some (Xmlt.encoding_to_xmlm_encoding enc) in
  let open Result_monad.Syntax in
  let- str =
    try Ok (Zip.find_entry ic "xl/sharedStrings.xml" |> Zip.read_entry ic) with
    | Not_found -> Error (`Msg "Could not find sharedStrings.xml")
    | Zip.Error (_, _, msg) -> Error (`Msg (Format.sprintf "Camlzip: %s" msg))
  in
  Xmlm.make_input ~enc:xmlm_enc (`String (0, str))
  |> get_xml
  |> accumulate_shared_strings ~enc []
  |> List.rev

let sheet_prefix = "xl/worksheets/sheet"
