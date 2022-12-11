open Core

(* Notice that we don't need to track the full board hyperstates here because we have all the moves to this position and will have a javascript based chess script for checking *)

type location = (char * int) [@@deriving eq, sexp];;

let string_of_location (loc: location): string =
  match loc with
  | c, r ->
    String.of_char_list [c;Helper.int_to_char r]

type direction = N | NE | E | SE | S | SW | W | NW [@@deriving eq];;

type meta = Pawn of {ep: bool} | King of {ck: bool;cq: bool} | Other;;

type piece = 
  {piece:Parser.piece;
   side:Parser.side;
   location:location;
   meta:meta}
;;

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


type square =
    | Empty of location
    | Occupied of location * piece;;

type plain_board = square list list;;

let get_square (board: plain_board) (c: char) (r: int): square =
  List.nth_exn (List.nth_exn board (r - 1)) (col_to_int c - 1)
 
let set_square (board: plain_board) (square: square): plain_board =
  (* Get information is square *)
  let lc, lr = match square with
    | Empty loc -> loc
    | Occupied (loc, _) -> loc
  in
  let rec set_board_pos (ic: int) (ir: int) (square: square) (board: plain_board): plain_board =
    if ir > 1 then
      match board with
      | h::tl -> h::(set_board_pos ic (ir - 1) square tl)
      | [] -> board
    else
      let rec set_square_list_pos (pos: int) (square: square) (sl: square list): square list =
        if pos > 1 then
          match sl with
          | h::tl -> h::(set_square_list_pos (pos - 1) (square) tl)
          | [] -> sl
        else
          match sl with
          | _::tl -> square::tl
          | [] -> sl
      in
      match board with
      | h::tl ->
        (set_square_list_pos ic square h)::tl
      | [] -> board
  in
  let lci = col_to_int lc in
  set_board_pos lci lr square board

type board = {
  board: plain_board;
  white_pieces: piece list;
  black_pieces: piece list;
  side_to_play: Parser.side;
}

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
    {piece=Knight;location=('b', 1);side=White;meta=Other};
    {piece=Bishop;location=('c', 1);side=White;meta=Other};
    {piece=Queen;location=('d', 1);side=White;meta=Other};
    {piece=King;location=('e', 1);side=White;meta=King {ck=true;cq=true}};
    {piece=Bishop;location=('f', 1);side=White;meta=Other};
    {piece=Knight;location=('g', 1);side=White;meta=Other};
    {piece=Rook;location=('h', 1);side=White;meta=Other};
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
    {piece=Knight;location=('b', 8);side=Black;meta=Other};
    {piece=Bishop;location=('c', 8);side=Black;meta=Other};
    {piece=Queen;location=('d', 8);side=Black;meta=Other};
    {piece=King;location=('e', 8);side=Black;meta=King {ck=true;cq=true}};
    {piece=Bishop;location=('f', 8);side=Black;meta=Other};
    {piece=Knight;location=('g', 8);side=Black;meta=Other};
    {piece=Rook;location=('h', 8);side=Black;meta=Other};
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
  let squares_2 = assign_piece_to_row_exn white_pawns 2 1 in
  let squares_1 = assign_piece_to_row_exn white_pieces 1 1 in
  let squares_7 = assign_piece_to_row_exn black_pawns 7 1 in
  let squares_8 = assign_piece_to_row_exn black_pieces 8 1 in
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
    let coli, row = coli + step * hm, row + step * vm in
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
                  match get_square board c r with
                  | Empty _ -> true
                  | Occupied _ -> false
                end
            in
            let checked_results = List.init ~f:(gen_loc_exn d (pc, pr)) (max (abs (row - pr)) (abs (col_to_int col - col_to_int pc)) - 1) in
            List.fold ~init:true ~f:(fun agg -> (fun x -> agg && x)) checked_results
        end
    end

let check_no_discovered_check (mv: Parser.mv) (piece: piece) (board: board): bool =
  (* 
     We are not checking checks from knight, because knight checks cannot be discovered
     The strategy is: identify the line (diagnal / row / col) defined by the piece and the side king.
     (If without that piece on that line no piece interferes a opponent checkable piece, then false)
  *)
  match piece.piece with
  | King -> true
  | _ -> (* It's not the king piece we need to find the king piece first *)
    begin
    let king = match piece.side with
      | White -> List.find_exn ~f:(fun x -> Parser.(equal_piece x.piece King)) board.white_pieces
      | Black -> List.find_exn ~f:(fun x -> Parser.(equal_piece x.piece King)) board.white_pieces
      | _ -> failwith "Root side is not an option for selected piece!"
    in
    let pc, pr = piece.location in
    let kc, kr = king.location in
    let pci, kci = (col_to_int pc), (col_to_int kc) in
    let d = direction_of_delta (pci - kci) (pr - kr) in
    let rec is_check_direction (loc: location) (avoid: location) (side: Parser.side) (d: direction) (board: plain_board): bool =
      (* This is the direction check function that examine whether there's check happening in a direction *)
      (* Avoid is the location of the piece that is about to move *)
      match loc with
      | pc, pr -> 
        (* First check whether on board this square is occupied by a checkable piece *)
        if equal_location loc avoid then
          match step_direction 1 d loc with
          | None -> false
          | Some loc ->  is_check_direction loc avoid side d board
        else
          begin
          match get_square board pc pr with
            | Empty _ -> 
              (* Defer to further squares *)
              begin
              match step_direction 1 d loc with
                | None -> false
                | Some loc -> is_check_direction loc avoid side d board
              end
            | Occupied (_, piece) ->
              begin
                if Parser.equal_side piece.side side then
                  false
                else
                  (* Here we know that side are different *)
                  match d with
                  | NW -> 
                    begin
                      match piece.piece with
                      | Queen -> true
                      | Bishop -> true
                      | _ -> false
                    end
                  | NE ->
                    begin
                      match piece.piece with
                      | Queen -> true
                      | Bishop -> true
                      | _ -> false
                    end
                  | SW ->
                    begin
                      match piece.piece with
                      | Queen -> true
                      | Bishop -> true
                      | _ -> false
                    end
                  | SE -> 
                    begin
                      match piece.piece with
                      | Queen -> true
                      | Bishop -> true
                      | _ -> false
                    end
                  | N ->
                    begin
                      match piece.piece with
                      | Rook -> true
                      | Queen -> true
                      | _ -> false
                    end
                  | S ->
                    begin
                      match piece.piece with
                      | Rook -> true
                      | Queen -> true
                      | _ -> false
                    end
                  | W ->
                    begin
                      match piece.piece with
                      | Rook -> true
                      | Queen -> true
                      | _ -> false
                    end
                  | E ->
                    begin
                      match piece.piece with
                      | Rook -> true
                      | Queen -> true
                      | _ -> false
                    end
              end
          end
      in
    (* One thing we also need to check is that whether the moved piece will remain on the same direction *)
    begin
      match mv.target with
      | None -> (not (is_check_direction king.location piece.location piece.side d board.board))
      | Some {col=col; row=row} ->
        let coli = col_to_int col in
        let dd = direction_of_delta (coli - kci) (row - kr) in
        (equal_direction dd d) || (not (is_check_direction king.location piece.location piece.side d board.board))
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
  (check_proper_move piece mv) && (check_no_piece_interference mv piece board.board) && (check_no_discovered_check mv piece board)

let rec update_piece_list (pl: piece list) (from_: location) (to_: location) (promotion: Parser.piece option): piece list =
  (* This update the piece list with a new piece position *)
  match pl with
  | [] -> pl
  | piece::tl ->
    if equal_location piece.location from_ then
      begin
        let meta = match piece.meta with
        | Pawn _ ->
          begin
            match from_, to_ with
            (_, fr), (_, tr) ->
              begin
                match promotion with
                | None ->
                  Pawn {ep=(abs(tr - fr) = 2)}
                | Some _ -> Other
              end
          end
        | King _ ->
          King {ck=false; cq=false}
        | _ -> piece.meta
        in
        let piece = {
          piece=(match promotion with None -> piece.piece | Some prm -> prm);
          location=to_;
          side=piece.side;
          meta=meta;
        } in
        piece::tl
      end
    else
      piece::(update_piece_list tl from_ to_ promotion)

let rec disable_castling (pl: piece list) (kq: char): piece list = 
  match pl with
  | [] -> []
  | piece::tl ->
    begin
      match piece.piece with
      | King ->
        {piece=King;
          location=piece.location;
          side=piece.side;
          meta=begin match piece.meta with
          | King {ck=ck;cq=cq} ->
            King {ck=(Char.(=) kq 'k') && ck;cq=(Char.(=) kq 'q') && cq}
          | _ -> failwith "Incorrect meta value!"
          end
        }::tl
      | _ -> piece::(disable_castling tl kq)
    end

let rec take_piece (pl: piece list) (loc: location): piece list =
  (* This function takes a board position and takes a piece off that position *)
  (* Notice that we don't have to modify the board because set_board_position will do it for us *)
  match pl with
  | [] -> []
  | h::tl -> 
    begin
      if equal_location h.location loc then
        tl
      else
        h::(take_piece tl loc)
    end

let set_piece_position (from_: location) (to_: location) (board: board) (flip_side: bool) (promotion: Parser.piece option): board =
  (* This set the piece form position from_ to position to_ *)
  (* First if we are moving anything from or to a1 or h1 disable castling for white, a8 or h8 for black *)
  let board =
    {
      board=board.board;
      side_to_play=board.side_to_play;
      white_pieces=if (equal_location from_ ('a', 1)) || (equal_location to_ ('a', 1)) then disable_castling board.white_pieces 'q' else board.white_pieces;
      black_pieces=if (equal_location from_ ('a', 8)) || (equal_location to_ ('a', 8)) then disable_castling board.black_pieces 'q' else board.black_pieces;
    }
  in
  let board =
    {
      board=board.board;
      side_to_play=board.side_to_play;
      white_pieces=if (equal_location from_ ('h', 1)) || (equal_location to_ ('h', 1)) then disable_castling board.white_pieces 'k' else board.white_pieces;
      black_pieces=if (equal_location from_ ('h', 8)) || (equal_location to_ ('h', 8)) then disable_castling board.black_pieces 'k' else board.black_pieces;
    }
  in
  match from_ with
  c, r ->
    match get_square board.board c r with
    | Empty _ -> failwith "No piece at the starting position!"
    | Occupied (_, piece) -> 
      (* construct a new square *)
      let source_square, target_square = (Empty from_, Occupied (to_, {piece=(match promotion with None -> piece.piece | Some prm -> prm);location=to_;side=piece.side;meta=(match promotion with None -> piece.meta | Some _ -> Other)})) in
      let updated_plain_board = set_square (set_square board.board source_square) target_square in
      (* Also need to update the piece storage information *)
      match board.side_to_play with
      (* Notice that we do not trust the take notation because some chessplayer does not use them. *)
      | White -> {
        board=updated_plain_board;
        white_pieces=update_piece_list board.white_pieces from_ to_ promotion;
        black_pieces=take_piece board.black_pieces to_;
        side_to_play=if flip_side then Black else White;
      }
      | Black -> {
        board=updated_plain_board;
        white_pieces=take_piece board.white_pieces to_;
        black_pieces=update_piece_list board.black_pieces from_ to_ promotion;
        side_to_play=if flip_side then White else Black;
      }
      | _ -> failwith "Nothing to do with Root side!"

let rec move_piece (mv: Parser.mv) (pl: piece list) (board: board): board =
  (* return a piece that can be move for this move *)
  match pl with
  | [] -> board
  | h::tl ->
    if mv.is_castle_k then
      (* 
         We only need to do king-side castle,
         King -> g1 (g8)
         Rook -> f1 (f8)
      *)
      match board.side_to_play with
      | White -> set_piece_position ('h', 1) ('f', 1) (set_piece_position ('e', 1) ('g', 1) board false None) true None
      | Black -> set_piece_position ('h', 8) ('f', 8) (set_piece_position ('e', 8) ('g', 8) board false None) true None
      | _ -> failwith "Never move for root!"
    else if mv.is_castle_q then
      (* We only need to do queen-side castle *)
      (* 
         We only need to do king-side castle,
         King -> c1 (c8)
         Rook -> d1 (d8)
      *)
      match board.side_to_play with
      | White -> set_piece_position ('a', 1) ('d', 1) (set_piece_position ('e', 1) ('c', 1) board false None) true None
      | Black -> set_piece_position ('a', 8) ('d', 8) (set_piece_position ('e', 8) ('c', 8) board false None) true None
      | _ -> failwith "Never move for root!"
    else if (Parser.equal_piece mv.piece h.piece) && (check_move_validity mv h board) then
      (* Move the piece here *)
      let _ = ignore(Printf.printf "%s\n" (string_of_location h.location)) in
      match mv.target with
      | Some {col=col;row=row} ->
        begin
          set_piece_position (h.location) (col, row) board true mv.promote
        end
      | None -> failwith "Only castling moves don't have target annotation!"
    else
      move_piece mv tl board

let make_move (move: Parser.move) (board: board): board =
  (* 
     This is the core functionality of the board that takes in a board, and a move,
     and return the board after the move.
  *)
  if Parser.equal_side (move.side) (board.side_to_play) then
    (* Should be able to play out the move *)
    let mv = move.move in (* Notice that the piece annotation in mv is of Parser.piece *)
    let piece_list = match move.side with
    | White -> board.white_pieces
    | Black -> board.black_pieces
    | _ -> failwith "Cannot move for root side!"
    in
    move_piece mv piece_list board
  else 
    failwith "Cannot playout the given move because of playing side conflict!"

(* Below we define some board printing function that allows us to ezamine board state *)
let string_of_piece (piece: piece): string =
  let pstr = match piece.side with
  | Black -> begin
      match String.lowercase (Parser.string_of_piece piece.piece) with
      | "#" -> "%"
      | p -> p
    end
  | White -> Parser.string_of_piece piece.piece
  | _ -> failwith "cannot use the root side to initialize piece!" in
  String.concat ~sep:"" [pstr;":";string_of_location piece.location]

let rec piece_list_to_string_list (pl: piece list): string list =
  (* Try to generate a string for the list of pieces *)
  match pl with
  | [] -> []
  | p::tl -> (string_of_piece p) :: (piece_list_to_string_list tl)

let string_of_square (sqr: square): string = 
  (* Try to generate a square string *)
  match sqr with
  | Empty _ -> "[ ]"
  | Occupied (_, piece) -> 
    begin
    let pstr = match piece.side with
      | Black -> begin
          match String.lowercase (Parser.string_of_piece piece.piece) with
          | "#" -> "%"
          | p -> p
        end
      | White -> Parser.string_of_piece piece.piece
      | _ -> failwith "cannot use the root side to initialize piece!" in
      Printf.sprintf "[%s]" pstr
    end

let square_list_to_string_list (sl: square list): string =
  String.concat ~sep:"" (List.map ~f:string_of_square sl)

let plain_board_to_string_list (sll: plain_board): string =
  String.concat ~sep:"\r\n" (List.rev (List.map ~f:square_list_to_string_list sll))

(* let board_to_string (board: board): string = 
  (* This function should be very  useful in examine board state *)
  let white_piece_str = String.concat ~sep:" " ("White: " :: (piece_list_to_string_list board.white_pieces)) in
  let black_piece_str = String.concat ~sep:" " ("Black: " :: (piece_list_to_string_list board.black_pieces)) in
  let board_str = plain_board_to_string_list board.board in *)