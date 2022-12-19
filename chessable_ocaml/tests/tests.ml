(* open Core;; *)
open OUnit2;;
open Chessable_ocaml;;

let test_int_to_char _ =
  assert_equal Helper.(int_to_char 0) '0';
  assert_equal Helper.(int_to_char 1) '1';
  assert_equal Helper.(int_to_char 2) '2'

let test_filter_option _ =
  assert_equal Helper.(filter_option [Some 1;Some 2]) [1;2];
  assert_equal Helper.(filter_option [None;None;None]) [];
  assert_equal Helper.(filter_option [None;None;Some "hello";Some "good"]) ["hello";"good"]

let test_pprint_line _ =
  assert_equal Parser.(pprint_line {
    move={
      piece=Pawn;
      target=Some {col='c';row=4};
      start_spec=None;
      is_check=false;
      is_mate=false;
      is_take=false;
      is_castle_q=false;
      is_castle_k=false;
      remark=None;
      promote=None;
    };
    side=White;
    turn_id=1;
    comment=Some "Strong Move!";
    continuation=[];
  }) "1. c4";

  assert_equal Parser.(pprint_line {
    move={
      piece=Pawn;
      target=Some {col='c';row=5};
      start_spec=None;
      is_check=false;
      is_mate=false;
      is_take=false;
      is_castle_q=false;
      is_castle_k=false;
      remark=None;
      promote=None;
    };
    side=Black;
    turn_id=1;
    comment=Some "Strong Move!";
    continuation=[];
  }) "c5"

let test_col_of_int _ =
  assert_equal Board.(col_of_int 3) 'c';
  assert_equal Board.(col_of_int 4) 'd';
  assert_equal Board.(col_of_int 8) 'h'

let test_col_to_int _ =
  (* Invariant test *)
  assert_equal Board.(col_of_int (col_to_int 'c')) 'c';
  assert_equal Board.(col_of_int (col_to_int 'd')) 'd'

let test_get_square _ =
  let board = Board.initialize () in
  assert_equal (match Board.get_square board.board 'b' 2 with | Occupied (('b', 2), _) -> true | _ -> false) true;
  assert_equal (match Board.get_square board.board 'c' 5 with | Empty ('c', 5) -> true | _ -> false) true

let test_set_square _ =
  let board = Board.initialize () in
  assert_equal Board.(match get_square (set_square board.board (Empty ('c', 2))) 'c' 2 with | Empty ('c', 2) -> true | _ -> false) true;
  assert_equal Board.(match get_square (set_square board.board (Empty ('a', 1))) 'a' 1 with | Empty ('a', 1) -> true | _ -> false) true

let test_direction_of_delta _ =
  assert_equal Board.(direction_of_delta 2 2) Board.NE;
  assert_equal Board.(direction_of_delta (-1) (-1)) Board.SW;
  assert_equal Board.(direction_of_delta 1 0) Board.E;
  assert_equal Board.(direction_of_delta 0 1) Board.N

let test_step_direction _ =
  assert_equal Board.(step_direction 2 N ('b', 2)) (Some ('b', 4));
  assert_equal Board.(step_direction 2 S ('c', 2)) None;
  assert_equal Board.(step_direction 9 SE ('c', 2)) None;
  assert_equal Board.(step_direction 5 NE ('a', 1)) (Some ('f', 6))

let test_check_proper_move _ =
  assert_equal Board.(check_proper_move {piece=Knight;side=White;location=('b', 1);meta=Other} {
      piece=Knight;
      target=Some {col='c';row=3};
      start_spec=None;
      is_check=false;
      is_mate=false;
      is_take=false;
      is_castle_q=false;
      is_castle_k=false;
      remark=None;
      promote=None;
    }) true;

  assert_equal Board.(check_proper_move {piece=Knight;side=White;location=('b', 1);meta=Other} {
      piece=Knight;
      target=Some {col='c';row=4};
      start_spec=None;
      is_check=false;
      is_mate=false;
      is_take=false;
      is_castle_q=false;
      is_castle_k=false;
      remark=None;
      promote=None;
    }) false

let test_no_piece_interference _ =
  (* Test there's no piece interfere *)
  let board = Board.initialize () in
  assert_equal Board.(check_no_piece_interference {
      piece=Bishop;
      target=Some {col='g';row=5};
      start_spec=None;
      is_check=false;
      is_mate=false;
      is_take=false;
      is_castle_q=false;
      is_castle_k=false;
      remark=None;
      promote=None;
  } {piece=Bishop;side=White;location=('c', 1);meta=Other} board.board) false;
  let board = Board.initialize () in
  assert_equal Board.(check_no_piece_interference {
      piece=Knight;
      target=Some {col='g';row=1};
      start_spec=None;
      is_check=false;
      is_mate=false;
      is_take=false;
      is_castle_q=false;
      is_castle_k=false;
      remark=None;
      promote=None;
  } {piece=Knight;side=White;location=('f', 3);meta=Other} board.board) true

let test_set_piece_position _ =
  let board = Board.initialize () in
  assert_equal Board.(set_piece_position ('b', 1) ('c', 3) board true None).side_to_play Black;
  assert_equal Board.(match (get_square (set_piece_position ('b', 1) ('c', 3) board true None).board 'b' 1) with | Empty _ -> true | _ -> false) true

let parser_test = "Parser Tests" >: test_list [
  (* Test for parser is relatively limited as we are exposing a few functions. *)
  "Test Pprint Line" >:: test_pprint_line;
]

let helper_test = "Helper Tests" >: test_list [
  "Test Int to Char" >:: test_int_to_char;
  "Test Filter Option" >:: test_filter_option;
]

let board_test = "Board Tests" >: test_list [
  "Test Col of Int" >:: test_col_of_int;
  "Test Col to Int" >:: test_col_to_int;
  "Test Get Square" >:: test_get_square;
  "Test Set Square" >:: test_set_square;
  "Test Direction of Delta" >:: test_direction_of_delta;
  "Test Step Direction" >:: test_step_direction;
  "Test Check Proper Move" >:: test_check_proper_move;
  "Test Check no Piece Interference" >:: test_no_piece_interference;
  "Test Set Piece Position" >:: test_set_piece_position;
]

let series = "Chessable_ocaml Tests" >::: [
  helper_test;
  parser_test;
  board_test;
]

let () =
  run_test_tt_main series