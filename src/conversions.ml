open Operations
open Result_monad.Infix
open Result_monad.Syntax

let convert_files ~ignore_hiddens ~shared_strings x :
    (string, [ `Msg of string ]) result =
  let xml = get_xml x in
  xml |> get_dim ||> fun (i, j) ->
  let array = Array.make_matrix (i + 1) (j + 1) "" in
  xml_to_csv ~ignore_hiddens shared_strings array (0, 0) xml;
  Array.map (fun arr -> arr |> Array.to_list |> String.concat ",") array
  |> Array.to_list |> String.concat "\n"

let to_csv_strings ~enc ~ignore_hiddens path :
    ((string, [ `Msg of string ]) result list, [ `Msg of string ]) result =
  let xmlm_enc = Some (Xmlt.encoding_to_xmlm_encoding enc) in
  let| ic =
    try Ok (Zip.open_in path)
    with _ -> Error (`Msg "Camlzip: Could not open file for reading")
  in
  let values =
    Zip.entries ic
    |> List.filter (fun f ->
           String.starts_with ~prefix:sheet_prefix Zip.(f.filename))
    |> List.map (fun f ->
           let str = Zip.read_entry ic f in
           Xmlm.make_input ~enc:xmlm_enc (`String (0, str)))
  in
  (if List.length values = 0 then Error (`Msg "No excel sheets to convert")
   else Ok ())
  >>| fun _ ->
  let- shared_strings = get_shared_strings ~enc ic in
  List.map (convert_files ~ignore_hiddens ~shared_strings) values

let to_csv_files ~enc ~ignore_hiddens in_path out_path :
    ((unit, [ `Msg of string ]) result list, [ `Msg of string ]) result =
  let xmlm_enc = Some (Xmlt.encoding_to_xmlm_encoding enc) in
  let out_path = fst out_path ^ snd out_path in
  let| ic =
    try Ok (Zip.open_in in_path)
    with _ -> Error (`Msg "Camlzip: Could not open file for reading")
  in
  let sheets =
    Zip.entries ic
    |> List.filter (fun f ->
           String.starts_with ~prefix:sheet_prefix Zip.(f.filename))
  in
  let values =
    sheets
    |> List.map (fun f ->
           let str = Zip.read_entry ic f in
           Xmlm.make_input ~enc:xmlm_enc (`String (0, str)))
  in
  (if List.length values = 0 then Error (`Msg "No excel sheets to convert")
   else Ok ())
  >>| fun _ ->
  let output_files =
    if List.length sheets = 1 then [ out_path ^ ".csv" ]
    else List.mapi (fun i _ -> out_path ^ "_" ^ string_of_int i ^ ".csv") sheets
  in
  (* create folders *)
  let op = out_path |> String.split_on_char '/' in
  let len = List.length op in
  op
  |> List.fold_left
       (fun acc f ->
         match acc with
         | Ok (i, acc) -> begin
             if i = len - 1 then Ok (i, acc)
             else
               let folder = if i = 0 then f else acc ^ "/" ^ f in
               begin
                 try Ok (Sys.mkdir folder 0o777) with
                 | Sys_error msg when msg = folder ^ ": File exists" -> Ok ()
                 | Sys_error msg ->
                     Error (`Msg (Format.sprintf "System: %s" msg))
               end
               >>| fun _ -> Ok (i + 1, folder)
           end
         | Error _ as err -> err)
       (Ok (0, ""))
  >>| fun _ ->
  let- shared_strings = get_shared_strings ~enc ic in
  List.map (convert_files ~ignore_hiddens ~shared_strings) values
  |> List.map2
       (fun name res ->
         let- content = res in
         let file = open_out name in
         output_string file content)
       output_files
