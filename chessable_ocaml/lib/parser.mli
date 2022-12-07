(* This module should define some basic parser action of the pgn files *)

(* Notice that the move type in this module is different from the move type in the repertoire*)

type side = White | Black | Root ;;
val equal_side: side -> side -> bool;;

type piece =
    Pawn (* indicate whether a pawn can be captured en passant *)
  | Knight
  | Bishop
  | Rook
  | Queen
  | King;; (* also indicate the avialability of castling either side *)
val equal_piece: piece -> piece -> bool;;
val string_of_piece: piece -> string;;

type square = {
    col: char;
    row: int;
};;

type mv = {
  piece: piece;
  target: square option;
  start_spec: char option;
  is_check: bool;
  is_mate: bool;
  is_take: bool;
  is_castle_q: bool;
  is_castle_k: bool;
  remark: string option;
  promote: piece option; (* Could have no promotion *)
};;

type move = {
  side: side;
  move: mv;
  turn_id: int;
  comment: string option;
  continuation: move list
};;

type conclusion;;

type tag = {
  key: string;
  value: string;
}

(* lines will always start with the root move *)
type pgn = {
  tags:tag list;
  lines: move;
  result: conclusion
}

val parse_pgn_file: string -> pgn;;

val pprint_line: move -> string;;