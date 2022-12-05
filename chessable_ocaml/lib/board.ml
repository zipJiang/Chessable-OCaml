open Core

(* Notice that we don't need to track the full board hyperstates here because we have all the moves to this position and will have a javascript based chess script for checking *)

type location = (char * int);;

type side =
  | White
  | Black

 type piece = 
    | Pawn of location * side * bool
    | Knight of location * side
    | Bishop of location * side
    | Rook of location * side
    | Queen of location * side
    | King of (location * side * bool * bool) (* This only indicates the basic castling right, need to also consder checks sqaure guards etc. *)
    [@@deriving sexp];;

type square =
    | Empty of location
    | Occupied of location * piece;;

type plain_board = square list list;;


type board = {
  board: plain_board;
  white_pieces: piece list;
  black_pieces: piece list;
  side_to_play: side;
}

let col_of_int (i: int): char =
  match i with
  | 1 -> 'a'
  | 2 -> 'b'
  | 3 -> 'c'
  | 4 -> 'd'
  | 5 -> 'e'
  | 6 -> 'f'
  | 7 -> 'g'
  | 8 -> 'h'
  | _ -> failwith "Invalid integer!"

let col_to_int (r: char): int =
  match r with
  | 'a' -> 1
  | 'b' -> 2
  | 'c' -> 3
  | 'd' -> 4
  | 'e' -> 5
  | 'f' -> 6
  | 'g' -> 7
  | 'h' -> 8
  | _ -> failwith "Invalid column indicator"

let initialize: board =
  (* This function will initialize a board to a playable state *)
  let white_pawns = [
    Pawn (('a', 2), White, false);
    Pawn (('b', 2), White, false);
    Pawn (('c', 2), White, false);
    Pawn (('d', 2), White, false);
    Pawn (('e', 2), White, false);
    Pawn (('f', 2), White, false);
    Pawn (('g', 2), White, false);
    Pawn (('h', 2), White, false);
  ] in
  let white_pieces = [
    Rook (('a', 1), White);
    Rook (('h', 1), White);
    Knight (('b', 1), White);
    Knight (('g', 1), White);
    Bishop (('c', 1), White);
    Bishop (('f', 1), White);
    King (('e', 1), White, true, true);
    Queen (('d', 1), White);
  ] in
  let black_pawns = [
    Pawn (('a', 7), Black, false);
    Pawn (('b', 7), Black, false);
    Pawn (('c', 7), Black, false);
    Pawn (('d', 7), Black, false);
    Pawn (('e', 7), Black, false);
    Pawn (('f', 7), Black, false);
    Pawn (('g', 7), Black, false);
    Pawn (('h', 7), Black, false);
  ] in
  let black_pieces = [
    Rook (('a', 8), Black);
    Rook (('h', 8), Black);
    Knight (('b', 8), Black);
    Knight (('g', 8), Black);
    Bishop (('c', 8), Black);
    Bishop (('f', 8), Black);
    King (('e', 8), Black, true, true);
    Queen (('d', 8), Black);
  ] in
  let rec assign_piece_to_row_exn (pl: piece list) (r: int) (c: int): square list =
    (* Assign 8 pieces to a list of row *)
    match pl with
    | [] -> []
    | h::tl -> (Occupied ((col_of_int c, r), h)) :: (assign_piece_to_row_exn tl r (c + 1))
  in
  let rec rows_of_empty_squares (r: int) (n: int): square list =
    match n with
    | 0 -> []
    | _ -> (Empty (col_of_int (9 - n), r)) :: (rows_of_empty_squares r (n - 1))
  in
  let squares_2 = assign_piece_to_row_exn white_pawns 2 0 in
  let squares_1 = assign_piece_to_row_exn white_pieces 1 0 in
  let squares_7 = assign_piece_to_row_exn black_pawns 7 0 in
  let squares_8 = assign_piece_to_row_exn black_pieces 8 0 in
  let plain_board = [
    squares_1;
    squares_2;
    rows_of_empty_squares 3 8;
    rows_of_empty_squares 4 8;
    rows_of_empty_squares 5 8;
    rows_of_empty_squares 6 8;
    squares_7;
    squares_8;
  ] in

  {
    board=plain_board;
    white_pieces=white_pawns @ white_pieces;
    black_pieces=black_pawns @ black_pieces;
    side_to_play=White
  }

let make_move (move: Parser.move) (board: board): board =
  (* 
     This is the core functionality of the board that takes in a board, and a move,
     and return the board after the move.
  *)
