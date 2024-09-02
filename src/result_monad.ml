module Infix = struct
  (* bind *)
  let ( >>| ) res f = match res with Ok v -> f v | Error _ as err -> err

  (* map (without error) *)
  let ( ||> ) res f = res >>| fun x -> Ok (f x)
end

module Syntax = struct
  open Infix

  (* bind *)
  let ( let| ) res f = res >>| f

  (* map (without error) *)
  let ( let- ) res f = res ||> f
end
