type into = [ `Strings | `Files of string * string ]

type outo =
  [ `Strings of (string, [ `Msg of string ]) result list
  | `Files of (unit, [ `Msg of string ]) result list ]

type encoding = [ `UTF_8 | `UTF_16 | `UTF_16BE | `UTF_16LE ]

module Infix : sig
  val ( >>| ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
  val ( ||> ) : ('a, 'b) result -> ('a -> 'c) -> ('c, 'b) result
end

module Syntax : sig
  val ( let| ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
  val ( let- ) : ('a, 'b) result -> ('a -> 'c) -> ('c, 'b) result
end

val convert :
  ?enc:encoding ->
  ?ignore_hiddens:bool ->
  into:into ->
  string ->
  (outo, [ `Msg of string ]) result
