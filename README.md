# xlsx2csv

Simple library to export .xlsx files as CSVs

## Basic usage

File into a list of strings:
```ocaml
let csvs = Xlsx2csv.convert ~into:`Strings "/path/to/your/excel_file.xlsx" in
match csvs with
| Ok (`Strings lst) ->
    List.iteri
      (fun i r ->
        let d = Format.dprintf "Sheet %d" (i + 1) in
        match r with
        | Ok str -> Format.printf "%t:@.%s@.@." d str
        | Error (`Msg msg) -> Format.printf "ERROR %t:@.%s@.@." d msg)
      lst
| Error (`Msg msg) -> failwith msg
| _ -> assert false
```

File into more files:
```ocaml
let files =
  Xlsx2csv.convert
    ~enc:(Some `UTF_16)
    ~ignore_hiddens:true
    ~into:(`Files ("/path/to/output/folder", "file_name"))
    "/path/to/your/excel_file.xlsx"
in
match files with
| Ok (`Files lst) ->
    List.iteri
      (fun i r ->
        let d = Format.dprintf "Sheet %d" (i + 1) in
        match r with
        | Ok () -> Format.printf "%t:@.OK@.@." d
        | Error (`Msg msg) -> Format.printf "ERROR %t:@.%s@.@." d msg)
      lst
| Error (`Msg msg) -> failwith msg
| _ -> assert false
```
