(* This module should define some basic parser action of the pgn files *)

(* Notice that the move type in this module is different from the move type in the repertoire*)
type move

type turn = move * move * int

(* Type tag is the beginning comment of the pgn file *)
type tag = Tag of string * string

type pgn

val parse_file: string -> pgn