(* Repertoire implementation based on the parser and the board *)
open Core

type transition_index = int * int[@@deriving sexp, yojson];;

(* This move type is without continuation *)
type move = {
  move: Parser.mv;
  side: Parser.side;
  turn_id: int;
  comment: string option;
}[@@deriving yojson];;

let move_from_parser_move (pmove: Parser.move): move =
  {
    move=pmove.move;
    side=pmove.side;
    turn_id=pmove.turn_id;
    comment=pmove.comment
  }

let parser_move_of_move (move: move): Parser.move =
  {
    move=move.move;
    side=move.side;
    turn_id=move.turn_id;
    comment=move.comment;
    continuation=[];
  }

type transition = {
  transition_id: int;
  from_: int;
  to_: int;
  move: move
}[@@deriving yojson];;

(* Observation is that after a move is made we do not need to actually store the board state *)
type rnode = {
  node_id: int;
  (* board: Board.board; *)
  board_hash: string;
  mutable continuations: int list;
}[@@deriving yojson];;

(* The first challenge is to implement the rnode and transition *)

module NodeMap = Map.Make(String);;


module Transition: (Map.Key with type t = transition_index) = struct
  type t = int * int;;
  let compare (a: t) (b: t): int =
    match a, b with
    | (af, at), (bf, bt) -> 
      if af = bf then
        at - bt
      else
        af - bf

  let sexp_of_t = sexp_of_transition_index;;
  let t_of_sexp = transition_index_of_sexp;;

end

module TransitionMap = Map.Make(Transition);;

type repetition = {
  level: int;
  last_seen: int; (* This may subject to refactorization *)
}[@@deriving yojson];;

type line = {
  from_node: int; (* The starting rnode index *)
  line: int list; (* We decide to index line as a sequence of transition index *)
  repetition: repetition;
}[@@deriving yojson];;

(* repertoire contains these annoying mapps that does not allow easy yojson, so we implement a portable repertoire type that can be easily yojsoned *)

type portable_repertoire = {
  lines: line list;
  nodes: rnode list;
  transitions: transition list;
}[@@deriving yojson];;

type repertoire = {
  mutable node_map: int NodeMap.t;
  mutable transition_map: int TransitionMap.t;
  mutable lines: line list;
  mutable nodes: rnode list;
  mutable transitions: transition list;
};;

let repertoire_to_portable (rptr: repertoire): portable_repertoire =
  (* Convert the repertoire to a portable format *)
  {
    lines=rptr.lines;
    nodes=rptr.nodes;
    transitions=rptr.transitions;
  }

let repertoire_of_portable (prptr: portable_repertoire): repertoire =
  (* This is a little bit complex, which require us to construct the map for transition and nodes *)
  {
    node_map=List.fold ~init:NodeMap.empty ~f:(fun m -> (fun n -> NodeMap.add_exn m ~key:n.board_hash ~data:n.node_id)) prptr.nodes;
    transition_map=List.fold ~init:TransitionMap.empty ~f:(fun m -> (fun t -> TransitionMap.add_exn m ~key:(t.from_, t.to_) ~data:t.transition_id)) prptr.transitions;
    lines=prptr.lines;
    nodes=prptr.nodes;
    transitions=prptr.transitions;
  }

let make_node (id: int) (board_hash: string): (rnode * int) =
  (* Make an rnode with empty continuation *)
  {
    node_id=id;
    board_hash=board_hash;
    continuations=[]
  }, id

let find_or_insert_node (rptr: repertoire) (board_hash: string): int =
  match NodeMap.find rptr.node_map board_hash with
  | None -> let node, nid = make_node (List.length rptr.nodes) board_hash in
    ignore(rptr.nodes <- (node::rptr.nodes));
    (* We also need to insert the node into the map *)
    ignore(rptr.node_map <- (NodeMap.add_exn ~key:board_hash ~data:nid rptr.node_map));
    nid
  | Some node_id -> node_id

let find_or_insert_transition (rptr: repertoire) (from_: int) (to_: int) (move: move): int =
  (* This is similar function for transitions *)
  match TransitionMap.find rptr.transition_map (from_, to_) with
  | None -> 
    let tr_id = List.length rptr.transitions in
    let tr = {transition_id=tr_id;from_=from_;to_=to_;move=move} in
    ignore(rptr.transitions <- tr::rptr.transitions);
    ignore(rptr.transition_map <- (TransitionMap.add_exn ~key:(from_, to_) ~data:tr_id rptr.transition_map));
    tr_id
  | Some tr_id -> tr_id

let append_transition_to_node (rptr: repertoire) (transition_id: int) (node_id: int) =
  (* This function will append transition to a node position *)
  let rec append_transition_to_nth_node (n: int) (tid: int) (node_list: rnode list): rnode list =
    match node_list with
    | [] -> []
    | h::tl ->
      if n = 0 then
        begin
        ignore(h.continuations <- tid::h.continuations);
        h::tl
        end
      else
        h::(append_transition_to_nth_node (n - 1) tid tl)
  in
  rptr.nodes <- (append_transition_to_nth_node node_id transition_id rptr.nodes)

let insert_move_from_position (rptr: repertoire) (board: Board.board) (move: move): repertoire =
  (* You can only insert move because move does not require position checking *)
  let board_after_move = Board.make_move (parser_move_of_move move) board in
  let before_idx = find_or_insert_node rptr (Board.to_fen board) in
  let after_idx = find_or_insert_node rptr (Board.to_fen board_after_move) in
  let transition_id = find_or_insert_transition rptr before_idx after_idx move in
  (* Of course we need to append the transition_id to the before rnode's continuation list *)
  ignore(append_transition_to_node rptr transition_id before_idx);
  rptr

let reindex_line (base: repertoire) (targ: repertoire) (line: line): line =
  (* This is the function that maps a line into new rnode index *)
  match line with {from_node=findex;line=int_list;repetition=repetition} ->
    let board_reindex (integer: int): int =
      NodeMap.find_exn (targ.node_map) (List.nth_exn base.nodes integer).board_hash in
    let nfindex = board_reindex (findex) in
    (* Transition index can be indentified from *)
    let transition_reindex (integer: int): int =
      let nf, nt = match List.nth_exn base.transitions integer with
      | {from_=from_; to_=to_;_} -> board_reindex from_, board_reindex to_ in
      TransitionMap.find_exn (targ.transition_map) (nf, nt)
    in
    {from_node=nfindex;line=List.map ~f:transition_reindex int_list;repetition=repetition}
 
let combine_repertoire (rptr: repertoire) (other: repertoire): repertoire =
  (* This function iterates over all positions and continuations from other and adding all transitions *)
  let merge_transition (rptr: repertoire) (node_list: rnode list) (tr: transition): unit =
    match tr with {move=move;from_=from_;to_=to_;_} ->
      let before_hash, after_hash = (List.nth_exn node_list from_).board_hash, (List.nth_exn node_list to_).board_hash in
      let before_idx, after_idx = find_or_insert_node rptr before_hash, find_or_insert_node rptr after_hash in
      ignore(find_or_insert_transition rptr before_idx after_idx move)
    in
  let merge_all_transition_from_node (rptr: repertoire) (tlist: transition list) (node: rnode): unit =
      List.iter ~f:(merge_transition rptr other.nodes) (List.map ~f:(List.nth_exn tlist) node.continuations)
    in
  List.iter ~f:(merge_all_transition_from_node rptr other.transitions) other.nodes;
  (* We also need to reindex all the lines in the original repertoire *)
  rptr.lines <- (rptr.lines @ List.map ~f:(reindex_line other rptr) other.lines);
  rptr

let rec merge_move_tree (start_board: Board.board) (rptr: repertoire) (move: Parser.move): repertoire =
  (* 
    Notice that merge_move_tree should take a tree style move in Parser.move,
    and try to merge every move in the move to the move repertoire tree.
  *)
  
  (* First we start by adding start_board position into the rptr *)
  let before_idx = find_or_insert_node rptr (Board.to_fen start_board) in
  let board_after_move = Board.make_move move start_board in
  let after_idx = find_or_insert_node rptr (Board.to_fen board_after_move) in
  (* construct a transition *)
  let _ = find_or_insert_transition rptr before_idx after_idx (move_from_parser_move move) in
  (* iterate over the continuaitons and do similar addition *)
  List.fold ~f:(merge_move_tree board_after_move) ~init:rptr move.continuation

(* From this line on we define json serialization functions for repertoire type with yojson functionality for portable_repertoire *)
let save_repertoire_to_file (rptr: repertoire) (filename: string): unit =
  let prptr = repertoire_to_portable rptr in
  let out_channel = Out_channel.create filename in
  Yojson.Safe.pretty_to_channel out_channel (portable_repertoire_to_yojson prptr)

let load_repertoire_from_file (filename: string): repertoire =
  (* need to load a portable_repertoire and create repertoire *)
  let prptr = match portable_repertoire_of_yojson @@ Yojson.Safe.from_file filename with
  | Error e -> failwith e
  | Ok prptr -> prptr in
  repertoire_of_portable prptr

(* One reasonable way to create lines is to introduce lines where comments is given for a position *)
(* Another reasonable way is to require memorization of all branching nodes *)