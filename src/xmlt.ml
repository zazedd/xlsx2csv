module Nmap = Map.Make (struct
  type t = string

  let compare = String.compare
end)

type xmlt = [ `Node of string * string Nmap.t * xmlt list | `Data of string ]
type encoding = [ `UTF_8 | `UTF_16 | `UTF_16BE | `UTF_16LE ]

let encoding_to_xmlm_encoding : encoding -> Xmlm.encoding = function
  | `UTF_8 -> `UTF_8
  | `UTF_16 -> `UTF_16
  | `UTF_16LE -> `UTF_16LE
  | `UTF_16BE -> `UTF_16BE
