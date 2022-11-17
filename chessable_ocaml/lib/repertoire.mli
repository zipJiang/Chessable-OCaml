(* 
    This is the declaration of a repertoire class, where
    we need to track the moves that reaches this position,
    possibly moves that can be made at this position and
    the actual position at this moment.
 *)
 type r

 type piece =
    | Pawn of (char * bool) (* indicate whether a pawn can be captured en passant *)
    | Knight of char
    | Bishop of char
    | Rook of char
    | Queen of char
    | King of (char * bool * bool) (* also indicate the avialability of castling either side *)
    [@@deriving sexp];;

type square =
    | Empty of (int * int)
    | Occupied of (int * int) * piece

type move
type board
type rnode

(* Spaced repetition to be used with the current node (should be a delta) *)
type spaced

(* Should contain a starting rnode position and a list of continuations *)
type line = rnode * (string list)

(* A line map is a mapping from lines to spaced repetition intervals *)
type line_map

module type Board = sig
    (*
       The current idea of implementation is as a list of list of squares, with the
       a map from piece to their occupied square (this possibly isn't required).
    *)
    type t = board
    (* Return the fen of current position *)
    val to_fen: t -> string
    (* Construct a board from fen *)
    val of_fen: string -> t
    (* validate if a move is legal or not *)
    val validate: move -> bool
    (* Make a move on the board, notice that this is implemented as square to square move *)
    val move: move -> promotion:char option -> t
    (* This helps locate the piece on the board, notice that this could either be implemented as a 64 enumeration, or a map *)
    val text_move: color: char -> move: string -> t
end

module type RepertoireNode = sig
    (*
       A repertoire node is a board position and continuation information, for first round implementation
       we probably don't track parent status, but only possible continuations
    *)
    type t = rnode

    (* cont should be some continuation map from string (or move?) to repertoirenode *)
    type cont

    (* find available continuations to current position *)
    val continuation: t -> cont option
    (* Get potential comments to the current position *)
    val get_comment: t -> string option
    (* 
       append a continuation to the current position and return the modified position,
       Notice that we expect to only get move input, as the position can be deduced.
    *)
    val append: t -> string -> t
    val remove: t -> string -> t
end

(* takes two repertoire, and join them together *)
val join: r -> r -> r
(* 
   If the board position is present, generate an rnode and move list of rnode representing continuation
   Notice that if this is not a line that is already known, should generate a new line from this posiiton
    and gets its own spaced repetition status
*)
val sample_line_from_position: r -> board -> line option

val all_lines: r -> line list
(* return all lines that are due *)
val all_due_lines: r -> line list
val update_line: r -> line -> spaced -> r

(* adding a line from a position, the position most be known before hand *)
val add_line: r -> line -> r

(* interaction with pgn, or maybe we can try to explore other more effective parsing strategy (may be a graph based strategy could be better) *)
val of_pgn: string -> r
val to_pgn: r -> string