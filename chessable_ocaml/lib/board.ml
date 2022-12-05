open Core

(* Notice that we don't need to track the full board hyperstates here because we have all the moves to this position and will have a javascript based chess script for checking *)

type location = (char * int) [@@deriving eq, sexp];;

type direction = N | NE | E | SE | S | SW | W | NW;;

type meta = Pawn of {ep: bool} | King of {ck: bool;cq: bool} | Other;;

type piece = 
  {piece:Parser.piece;
   side:Parser.side;
   location:location;
   meta:meta}
;;

type square =
    | Empty of location
    | Occupied of location * piece;;

type plain_board = square list list;;


type board = {
  board: plain_board;
  white_pieces: piece list;
  black_pieces: piece list;
  side_to_play: Parser.side;
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
    {piece=Pawn; location=('a', 2); side=White; meta=Pawn {ep=false}};
    {piece=Pawn; location=('b', 2); side=White; meta=Pawn {ep=false}};
    {piece=Pawn; location=('c', 2); side=White; meta=Pawn {ep=false}};
    {piece=Pawn; location=('d', 2); side=White; meta=Pawn {ep=false}};
    {piece=Pawn; location=('e', 2); side=White; meta=Pawn {ep=false}};
    {piece=Pawn; location=('f', 2); side=White; meta=Pawn {ep=false}};
    {piece=Pawn; location=('g', 2); side=White; meta=Pawn {ep=false}};
    {piece=Pawn; location=('h', 2); side=White; meta=Pawn {ep=false}};
  ] in
  let white_pieces = [
    {piece=Rook;location=('a', 1);side=White;meta=Other};
    {piece=Rook;location=('h', 1);side=White;meta=Other};
    {piece=Knight;location=('b', 1);side=White;meta=Other};
    {piece=Knight;location=('g', 1);side=White;meta=Other};
    {piece=Bishop;location=('c', 1);side=White;meta=Other};
    {piece=Bishop;location=('f', 1);side=White;meta=Other};
    {piece=King;location=('e', 1);side=White;meta=King {ck=true;cq=true}};
    {piece=Queen;location=('d', 1);side=White;meta=Other};
  ] in
  let black_pawns = [
    {piece=Pawn; location=('a', 7); side=Black; meta=Pawn {ep=false}};
    {piece=Pawn; location=('b', 7); side=Black; meta=Pawn {ep=false}};
    {piece=Pawn; location=('c', 7); side=Black; meta=Pawn {ep=false}};
    {piece=Pawn; location=('d', 7); side=Black; meta=Pawn {ep=false}};
    {piece=Pawn; location=('e', 7); side=Black; meta=Pawn {ep=false}};
    {piece=Pawn; location=('f', 7); side=Black; meta=Pawn {ep=false}};
    {piece=Pawn; location=('g', 7); side=Black; meta=Pawn {ep=false}};
    {piece=Pawn; location=('h', 7); side=Black; meta=Pawn {ep=false}};
  ] in
  let black_pieces = [
    {piece=Rook;location=('a', 8);side=Black;meta=Other};
    {piece=Rook;location=('h', 8);side=Black;meta=Other};
    {piece=Knight;location=('b', 8);side=Black;meta=Other};
    {piece=Knight;location=('g', 8);side=Black;meta=Other};
    {piece=Bishop;location=('c', 8);side=Black;meta=Other};
    {piece=Bishop;location=('f', 8);side=Black;meta=Other};
    {piece=King;location=('e', 8);side=Black;meta=King {ck=true;cq=true}};
    {piece=Queen;location=('d', 8);side=Black;meta=Other};
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

let direction_of_delta (dc: int) (dr: int): direction =
  (* Swap direction back into square delta *)
  match dc, dr with
  | 0, 0 -> failwith "Not moved at all!"
  | 0, _ -> if dr < 0 then S else N
  | _, 0 -> if dc < 0 then W else E
  | _, _ -> 
    if dr < 0 && dc > 0 then
      SE
    else if dr < 0 && dc < 0 then
      SW
    else if dr > 0 && dc > 0 then
      NE
    else
      NW

let step_direction (step: int) (d: direction) (loc: location): location option =
  (* Moving starting from a direction and several steps to a direction *)
  match loc with
  | col, row -> 
    let coli = col_to_int col in
    begin
    let hm, vm = match d with
    | N -> (0, 1)
    | NE -> (1, 1)
    | E -> (1, 0)
    | SE -> (1, -1)
    | S -> (0, -1)
    | SW -> (-1, -1)
    | W -> (-1, 0)
    | NW -> (-1, 1)
    in
    let coli, row = coli + step * vm, row + step * hm in
    if (coli < 1 || coli > 8) || (row < 1 || row > 8) then
      None
    else
      Some (col_of_int coli, row)
    end

let compare_piece_type (pp: Parser.piece) (pb: piece): bool =
  (* Compare whether they are the same piece type *)
  Parser.equal_piece pb.piece pp


let check_proper_move (piece: piece) (mv: Parser.mv): bool =
  (* This checks condition 1) *)
  match piece.piece with
  | Pawn -> 
    if mv.is_take then
      (* 
          This is a take move, so it requires that the target square has piece, but this is not checked,
          Notice that this function only checks if the piece can move to the target square.
      *)
      match mv.target with
      | None -> false (* Cannot move pawn to None *)
      | Some {col=col;row=row} ->
        begin
          match piece.side with
          | White -> let pm1, pm2 = step_direction 1 NE piece.location, step_direction 1 NW piece.location in
            let potentials = Helper.filter_option [pm1;pm2] in
            begin
              match List.find ~f:(equal_location (col, row)) potentials with
              Some _ -> true
              | None -> false
            end
          | Black -> let pm1, pm2 = step_direction 1 SE piece.location, step_direction 1 SW piece.location in
            let potentials = Helper.filter_option [pm1;pm2] in
            begin
              match List.find ~f:(equal_location (col, row)) potentials with
              Some _ -> true
              | None -> false
            end
          | _ -> failwith "Does not require move for Root!"
        end
    else
      begin
      match mv.target with
      | None -> false
      | Some {col=col;row=row} ->
        begin
          (* We don't have to check whether the pawn can move 2 squares yet, because if two pawns satisify the criteria, one of the pawn must be blocked *)
          match piece.side with
          | White -> let pm1, pm2 = step_direction 1 N piece.location, step_direction 2 N piece.location in
            let potentials = Helper.filter_option [pm1;pm2] in
            begin
              match List.find ~f:(equal_location (col, row)) potentials with
              Some _ -> true
              | None -> false
            end
          | Black -> let pm1, pm2 = step_direction 1 S piece.location, step_direction 2 S piece.location in
            let potentials = Helper.filter_option [pm1;pm2] in
            begin
              match List.find ~f:(equal_location (col, row)) potentials with
              Some _ -> true
              | None -> false
            end
          | _ -> failwith "Does not require move for Root!"
        end
      end
  | Bishop ->
    (* Pieces are simplier because they only have fixed ways of moving *)
    begin
      match mv.target, piece.location with
      | None, _ -> false
      | Some {col=col;row=row}, (pc, pr) ->
        begin
          let coli = col_to_int col in
          let delta_x, delta_y = abs((col_to_int pc) - coli), abs(pr - row) in
          delta_x = delta_y
        end
    end
  | Knight ->
    (* Similar for knight *)
    begin
      match mv.target, piece.location with
      | None, _ -> false
      | Some {col=col;row=row}, (pc, pr) ->
        begin
          let coli = col_to_int col in
          let delta_x, delta_y = abs((col_to_int pc) - coli), abs(pr - row) in
          (delta_x = 2 * delta_y) || (delta_y = 2 * delta_x)
        end
    end
  | Rook -> 
    begin
      match mv.target, piece.location with
      | None, _ -> false
      | Some {col=col;row=row}, (pc, pr) ->
        begin
          let coli = col_to_int col in
          let delta_x, delta_y = abs((col_to_int pc) - coli), abs(pr - row) in
          (delta_x = 0) || (delta_y = 0)
        end
    end
  | Queen ->
    begin
      match mv.target, piece.location with
      | None, _ -> false
      | Some {col=col;row=row}, (pc, pr) ->
        begin
          let coli = col_to_int col in
          let delta_x, delta_y = abs((col_to_int pc) - coli), abs(pr - row) in
          (delta_x = 0) || (delta_y = 0) || (delta_x = delta_y)
        end
    end
  | King -> true (* There is only one king! *)

let check_no_piece_interference (mv: Parser.mv) (piece: piece) (board: plain_board): bool =
  (* This checks condition 2 *)
  match piece.piece with
  | Knight -> true(* Knight can move regardless *)
  | _ -> 
    begin
      match mv.target with
      | None -> false
      | Some {col=col; row=row} ->
        begin
          match piece.location with
          pc, pr -> 
            let d = direction_of_delta ((col_to_int col) - (col_to_int pc)) (row - pr) in
            let gen_loc_exn (dr: direction) (loc: location) (step_m: int): bool =
              let step = step_m + 1 in
              match step_direction step dr loc with
              | None -> failwith "This location should be valid but failed!"
              | Some (c, r) -> 
                begin
                  match List.nth_exn (List.nth_exn board (col_to_int c)) r with
                  | Empty _ -> true
                  | Occupied _ -> false
                end
            in
            let checked_results = List.init ~f:(gen_loc_exn d (pc, pr)) (abs (row - pr) - 1) in
            List.fold ~init:true ~f:(fun agg -> (fun x -> agg && x)) checked_results
        end
    end

let check_move_validity (mv: Parser.mv) (piece: piece) (board: board): bool =
  (*
    Following cases disallow a move:
    1) The piece is not able to be moved to the target square in 1 move with its way of moving.
    2) There is piece interferring between the target and the current location
    3) The move discovers a check by the opponent.

    again we are not checking exclusively only those that allows us to dicern pieces.
  *)
  | h::tl -> (* First check if h is the desired piece *)
    if compare_piece_type mv.piece h then
      (* 
         Check whether we are able to match further annotation.
         Notice that if no start_position annotation is given we
         identify piece with move validity.
      *)
      match mv.start_spec with
      | None -> 
        begin
          (* 
             Unfortunately need to match with move possibility
          *)
        end
      | Some c -> if (Char.(<=) c '8') && (Char.(>=) c '1') then
        (* Annotation is a row *)
        begin
        match h with
        | {piece=_;location=(_;row);side=_;meta=_} ->
          (* Now we know that piece matches, lets check whether location matches the annotation *)
          if (Helper.int_to_char c) = row then
            Some h
          else
            match_move_piece mv tl
        end
        else if  (Char.(<=) c 'h') && (Char.(>=) c 'a') then
          (* Annotation is a row *)
        begin
        match h with
        | {piece=_;location=(col;_);side=_;meta=_} ->
          if c = col then
            Some h
          else
            match_move_piece mv tl
        end
    else 
      match_move_piece mv tl


let make_move (move: Parser.move) (board: board): board =
  (* 
     This is the core functionality of the board that takes in a board, and a move,
     and return the board after the move.
  *)
  if Parser.equal_side (move.side) (board.side_to_play) then
    (* Should be able to play out the move *)
    let mv = move.move in (* Notice that the piece annotation in mv is of Parser.piece *)
  else 
    failwith "Cannot playout the given move because of playing side conflict!"