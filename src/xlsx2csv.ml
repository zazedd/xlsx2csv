type into = [ `Strings | `Files of string * string ]

type outo =
  [ `Strings of (string, [ `Msg of string ]) result list
  | `Files of (unit, [ `Msg of string ]) result list ]

type encoding = Xmlt.encoding

module Infix = Result_monad.Infix
module Syntax = Result_monad.Syntax
open Infix

(* encoding can probably be figured out by the
   xml header but i havent seen an xlsx file that does that *)
let convert ?(enc : encoding = `UTF_8) ?(ignore_hiddens = false) ~(into : into)
    path : (outo, [ `Msg of string ]) result =
  match into with
  | `Files s ->
      Conversions.to_csv_files ~enc ~ignore_hiddens path s ||> fun res ->
      `Files res
  | `Strings ->
      Conversions.to_csv_strings ~enc ~ignore_hiddens path ||> fun res ->
      `Strings res
