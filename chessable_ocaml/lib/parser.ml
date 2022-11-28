(* Actually implement the parser *)

open Core

type side = White | Black | Root;;

 type piece =
      Pawn (* indicate whether a pawn can be captured en passant *)
    | Knight
    | Bishop
    | Rook
    | Queen
    | King;; (* also indicate the avialability of castling either side *)

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

(* We could potentially allow continuation in comments, but that involves more complex designs *)
type move = {
  side: side;
  move: mv;
  turn_id: int;
  comment: string option;
  continuation: move list
};;

type conclusion = WhiteWin | BlackWin | Draw | Unknown;;
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

let int_to_char (i: int): char =
  match i with
  | 0 -> '0'
  | 1 -> '1'
  | 2 -> '2'
  | 3 -> '3'
  | 4 -> '4'
  | 5 -> '5'
  | 6 -> '6'
  | 7 -> '7'
  | 8 -> '8'
  | 9 -> '9'
  | _ -> failwith "number cannot be cast to single digit char."

let is_whitespace (c: char): bool =
  (* Helper function that checks white space. *)
  match c with
  | '\239' -> true
  | '\187' -> true
  | '\191' -> true
  | '\r' -> true
  | '\n' -> true
  | '\t' -> true
  | '\b' -> true
  | ' ' -> true
  | _ -> false

let rec drop_white_space_at_head (cl: char list): char list = 
  (* Helper function that that takes a char list and return the raminder *)
  match cl with
  | [] -> []
  | h::tl -> if is_whitespace h then drop_white_space_at_head tl else cl

let rec read_until (c: char) (cl: char list): (char list) * (char list) =
  (* This should be a very strightforward implementation of splitting on a given character *)
  match cl with
  | [] -> [], []
  | h::tl -> if Char.(=) h c then
    (* Notice that the specified character is removed *)
    [], tl
    else
      let read, remainder = read_until c tl in
      h::read, remainder


let parse_mv (cl: char list): mv * (char list) = 
  (*
    The move specification is a string that has partial elements
    [PIECE](ANNOT)(IS_TAKE)[COL][ROW](PROMO)(IS_CHECK_OR_MATE)[REMARK]

    PIECE: which piece (abcdefgh for pawns)
    ANNOT: to uniquely identify a piece [a-h0-9]
    IS_TAKE: 'x' or Not apply
    COL: [abcdefgh]
    ROW: [0-9]
    PROMO: [=x]
    IS_CHECK_OR_MATE [#+]
    REMARK [!;!!;?;??;!?;?!]

    should also allow ... to skip
  *)

  match cl with
  'O'::'-'::'O'::'-'::'O'::cl ->
    begin
      (* This is castling queenside *)
      let is_check, is_mate, cl = match cl with
      | '+'::tl -> true, false, tl
      | '#'::tl -> true, true, tl
      | _ -> false, false, cl
      in
      let remark, cl = match cl with
      | '!'::'!'::tl -> Some "!!", tl
      | '!'::'?'::tl -> Some "!?", tl
      | '?'::'!'::tl -> Some "?!", tl
      | '?'::'?'::tl -> Some "??", tl
      | '?'::tl -> Some "?", tl
      | '!'::tl -> Some "!", tl
      | _ -> None, cl
    in
      {piece=King;target=None;start_spec=None;is_take=false;is_castle_k=false;is_castle_q=true;is_check=is_check;is_mate=is_mate;remark=remark;promote=None}, cl
    end
  | 'O'::'-'::'O'::cl ->
    begin
      (* This is castling kingside *)
      let is_check, is_mate, cl = match cl with
      | '+'::tl -> true, false, tl
      | '#'::tl -> true, true, tl
      | _ -> false, false, cl
      in
      let remark, cl = match cl with
      | '!'::'!'::tl -> Some "!!", tl
      | '!'::'?'::tl -> Some "!?", tl
      | '?'::'!'::tl -> Some "?!", tl
      | '?'::'?'::tl -> Some "??", tl
      | '?'::tl -> Some "?", tl
      | '!'::tl -> Some "!", tl
      | _ -> None, cl
    in
      {piece=King;target=None;start_spec=None;is_take=false;is_castle_k=true;is_castle_q=false;is_check=is_check;is_mate=is_mate;remark=remark;promote=None}, cl
    end
  | _ ->
    begin
      begin
      (* 1. Parse piecename *)
      let piece, cl = match cl with
      | 'N'::tl -> Knight, tl
      | 'K'::tl -> King, tl
      | 'B'::tl -> Bishop, tl
      | 'Q'::tl -> Queen, tl
      | 'R'::tl -> Rook, tl
      | tl -> Pawn, tl
      in
      (* 2. Parse piece annotation, is_take and target square, the challenging stuff here is that we have to parse all four characters at the same time, because we don't know whether each of them is present *)
      let annot, is_take, target, cl = match cl with
      | a::b::tl -> begin
          if Char.(=) a 'x' then
            (* No annotation *)
            begin
              match b::tl with
              | c::r::tl ->
                None, true, {col=c; row=(int_of_string (String.of_char r))}, tl
              | _ -> failwith "Not enough input items!"
            end
          else
              begin
                if (Char.(<=) b '8') && (Char.(>=) b '1') then
                  (* There is no annotation and no taking *)
                  None, false, {col=a; row=(int_of_string (String.of_char b))}, tl
                else if (Char.(=) b 'x')  then
                  begin
                  match tl with
                  | c::r::tl ->
                    Some a, true, {col=c; row=(int_of_string (String.of_char r))}, tl
                  | _ -> failwith "Not enough input items!"
                  end
                else
                  (* Has annotation but not a take *)
                  begin
                    match b::tl with
                    | c::r::tl -> Some a, false, {col=c; row=(int_of_string (String.of_char r))}, tl
                    | _ -> failwith "Not enough input items!"
                  end
              end
        end
      | _ -> failwith "Not enough characters left!"
      in
      let promo, cl = match cl with
      | '='::'N'::tl -> Some Knight, tl
      | '='::'B'::tl -> Some Bishop, tl
      | '='::'R'::tl -> Some Rook, tl
      | '='::'Q'::tl -> Some Queen, tl
      | _ -> None, cl
      in
      let is_check, is_mate, cl = match cl with
      | '+'::tl -> true, false, tl
      | '#'::tl -> true, true, tl
      | _ -> false, false, cl
      in
      let remark, cl = match cl with
      | '!'::'!'::tl -> Some "!!", tl
      | '!'::'?'::tl -> Some "!?", tl
      | '?'::'!'::tl -> Some "?!", tl
      | '?'::'?'::tl -> Some "??", tl
      | '?'::tl -> Some "?", tl
      | '!'::tl -> Some "!", tl
      | _ -> None, cl
      in
        {piece=piece;target=Some target;start_spec=annot;is_take=is_take;is_castle_k=false;is_castle_q=false;is_check=is_check;is_mate=is_mate;remark=remark;promote=promo}, cl
      end
    end

let parse_comment (cl: char list): (string option) * (char list) =
  let cl = drop_white_space_at_head cl in
  match cl with
  | ';'::tl -> let comment, cl = read_until '\n' tl in
    Some (String.of_char_list comment), cl
  | '{'::tl -> let comment, cl = read_until '}' tl in
    Some (String.of_char_list comment), cl
  | _ -> None, cl

let rec parse_move (cl: char list) (side: side) (turn_id: int): (move option) * (char list) =
  (* 
    Also parsing comments and generate a move.
    The move will be added to the aggregated move linked list
  *)
  let cl = drop_white_space_at_head cl in
  match cl with
  | [] -> None, cl
  | '.'::'.'::'.'::tl -> begin
      parse_move tl Black turn_id
    end
  | h::_ ->
    if ((Char.(<=) h 'h') && (Char.(>=) h 'a')) || 
      (Char.(=) h 'K') || (Char.(=) h 'N') || (Char.(=) h 'B') || (Char.(=) h 'Q') || (Char.(=) h 'R') || (Char.(=) h 'O') then
      let mv, cl = parse_mv cl in
      (* also try to parse comment *)
      let comment, cl = parse_comment cl in
      Some {side=side;move=mv;turn_id=turn_id;comment=comment;continuation=[]}, cl
    else
      None, cl

let parse_turn_id (cl: char list): int * (char list) =
  (*
    Parse the xx. format of turn_id
  *)
  let cl = drop_white_space_at_head cl in
  let number_char_list, cl = read_until '.' cl in
  int_of_string (String.of_char_list number_char_list), cl


let rec parse_line (cl: char list) (parent: move): move * (char list) =
  (*
    This function still work with moves, but will return a move
    linked list that can be traced towards the end of the game.

    parent indicates which move the currently parsed move should be attached to to form line
  *)
  let cl = drop_white_space_at_head cl in
  match cl with
  | [] -> parent, []
  | ')'::tl ->
    (* This means that the parenthesis parsing should be over *)
    parent, tl
  | '1'::'-'::_ -> parent, cl
  | '0'::'-'::_ -> parent, cl
  | '1'::'/'::'2'::_ -> parent, cl
  | h::_ -> 
    begin
      if (Char.(>=) h '0') && (Char.(<=) h '9') then
        begin
        let turn_id, cl = parse_turn_id cl in
        (* Then we parse a move and recursively apply parse_line until the ) character is met *)
        (* Because if you see turn_id you assume it is white to move *)
        let move_opt, cl = parse_move cl White turn_id in
        match move_opt with
        | None -> parent, cl
        | Some move -> 
          let cl = drop_white_space_at_head cl in
          begin
            match cl with
            | '('::tl ->
              let parent, cl = parse_line tl parent in
              {side=parent.side;move=parent.move;turn_id=parent.turn_id;comment=parent.comment;continuation=move::parent.continuation}, cl
            | _ ->
              let move, cl = parse_line cl move in
              {side=parent.side;move=parent.move;turn_id=parent.turn_id;comment=parent.comment;continuation=move::parent.continuation}, cl
          end
        end
    else
      begin
        let turn_id = parent.turn_id in
        let move_opt, cl = parse_move  cl Black turn_id in
        match move_opt with
        | None -> parent, cl
        | Some move ->
          let cl = drop_white_space_at_head cl in
          begin
            match cl with
            | '('::tl ->
              let parent, cl = parse_line tl parent in
              {side=parent.side;move=parent.move;turn_id=parent.turn_id;comment=parent.comment;continuation=move::parent.continuation}, cl
            | _ ->
              let move, cl = parse_line cl move in
              {side=parent.side;move=parent.move;turn_id=parent.turn_id;comment=parent.comment;continuation=move::parent.continuation}, cl
          end
      end
    end


let parse_result (cl: char list): conclusion * (char list) =
  let cl = drop_white_space_at_head cl in
  match cl with
  | [] -> Unknown, cl
  | '*'::tl -> Unknown, tl
  | '1'::'/'::'2'::'-'::'1'::'/'::'2'::tl -> Draw, tl
  | '1'::'-'::'0'::tl -> WhiteWin, tl
  | '0'::'-'::'1'::tl -> BlackWin, tl
  | _ -> Unknown, cl

(* We also need a function to generate root node *)
let gen_root: move = 
    { side=Root;
      move={
        piece=King;
        target=None;
        start_spec=None;
        is_check=false;
        is_mate=false;
        is_take=false;
        is_castle_q=false;
        is_castle_k=false;
        remark=None;
        promote=None;
      };
      turn_id=0;
      comment=None;
      continuation=[]
    }

(* Now with a function to parse tags we should be able to implement the complete io parsing *)
let parse_tag (cl: char list): (tag option) * (char list) =
  (* This function will read the char list and generate tag items if possible *)
  let cl = drop_white_space_at_head cl in
  match cl with
  | '['::tl -> let key, tl = read_until ' ' tl in
    begin
      match tl with
      | '"'::tl -> let value, tl = read_until '"' tl in
        let _, tl = read_until ']' tl in
        Some {key=(String.of_char_list key);value=(String.of_char_list value)}, tl
      | _ -> failwith "error parsing value for a tag!"
    end
  | _ -> None, cl


let rec parse_all_tags (cl: char list): (tag list) * (char list) =
  (* Parse all tags in the front of a document *)
  let cl = drop_white_space_at_head cl in
  let tag_opt, cl = parse_tag cl in
  match tag_opt with
  | None -> [], cl
  | Some tag -> let tlist, cl = parse_all_tags cl in
    tag::tlist, cl

let parse_pgn_file (filename: string): pgn =
  let read_file (filename: string): string =
    In_channel.with_file ~f:In_channel.input_all filename in
  let content = String.to_list (read_file filename) in
  let tg_list, content = parse_all_tags content in
  let root = gen_root in
  (* Notice that before parse-line we should parse comments for the root *)
  let general_comment_opt, content = parse_comment content in
  let root = {
    side=root.side;
    move=root.move;
    turn_id=root.turn_id;
    comment=general_comment_opt;
    continuation=root.continuation
  } in
  let root, content = parse_line content root in
  let result, _ = parse_result content in
  {tags=tg_list;lines=root;result=result}

let move_to_text (move: move): string =
  let move_text = 
  if move.move.is_castle_k then
    "O-O"
  else if move.move.is_castle_q then
    "O-O-O"
  else
    let piece_text =
    match move.move.piece with
    | Pawn -> ""
    | Knight -> "N"
    | Bishop -> "B"
    | King -> "K"
    | Queen -> "Q"
    | Rook -> "R"
    in
    let starting_annotation_text =
    match move.move.start_spec with
    | None -> ""
    | Some c -> String.of_char c
    in
    let is_take_text =
    match move.move.is_take with
    | false -> ""
    | true -> "x"
    in
    let target_square_text =
    match move.move.target with
    | None -> ""
    | Some sqr -> String.of_char_list [sqr.col;int_to_char sqr.row]
    in
    let mate_check_text =
    if move.move.is_mate then
      "#"
    else if move.move.is_check then
      "+"
    else
      ""
    in
    let remark_text = 
    match move.move.remark with
    | None -> ""
    | Some s -> s
    in
    let promotion_text = 
    match move.move.promote with
    | Some Knight -> "=N"
    | Some Bishop -> "=B"
    | Some King -> "=K"
    | Some Queen -> "=Q"
    | Some Rook -> "=R"
    | None -> ""
    | _ -> failwith "Invalid promotion."
    in
    String.concat ~sep:"" [piece_text;starting_annotation_text;is_take_text;target_square_text;promotion_text;mate_check_text;remark_text]
  in
  begin
    match move.side with
    | White ->
      String.concat ~sep:". " [Printf.sprintf "%d" move.turn_id;move_text]
    | Black ->
        move_text
    | Root -> ""
  end
    

let rec pprint_line (move: move): string =
  (* This function will read a move and continuation *)
  match move.continuation with
  | [] -> (* This is the end move *) 
    move_to_text move
  | [main_cont] ->
      (* Having only one continuation *)
      String.concat ~sep:" " [(move_to_text move);(move_to_text main_cont)]
  | h::tl ->
      (* Having multiple continuations so that we first represent those that are not main continuations *)
      (* For simplicity the implementation reverse the order of main move and sidelines *)
      (move_to_text move)::(pprint_line h)::