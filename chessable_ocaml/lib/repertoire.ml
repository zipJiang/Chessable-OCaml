(* Repertoire implementation based on the parser and the board *)
open Core

type transition_index = int * int[@@deriving sexp];;

type transition = {
  transition_id: int;
  from_: int;
  to_: int;
  move: Parser.move (* TODO: We may want to apply a simplified move type, where we don't have the complete tree structure *)
};;

type rnode = {
  node_id: int;
  board: Board.board;
  board_hash: string;
  mutable continuations: int list;
};;

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
}

type line = {
  line: int list;
  repetition: repetition;
}

type repertoire = {
  mutable node_map: int NodeMap.t;
  mutable transition_map: int TransitionMap.t;
  mutable lines: line list;
  mutable nodes: rnode list;
  mutable transitions: transition list;
};;

let make_node (board: Board.board) (id: int) (board_hash: string option): (rnode * int) =
  (* Make an rnode with empty continuation *)
  match board_hash with
  | None -> {
    node_id=id;
    board=board;
    board_hash=Board.to_fen board;
    continuations=[];
  }, id
  | Some hash -> {
    node_id=id;
    board=board;
    board_hash=hash;
    continuations=[]
  }, id

let find_or_insert_node (rptr: repertoire) (board: Board.board): int =
  let board_hash = Board.to_fen board in
  match NodeMap.find rptr.node_map board_hash with
  | None -> let node, nid = make_node board (List.length rptr.nodes) (Some board_hash) in
    ignore(rptr.nodes <- (node::rptr.nodes));
    (* We also need to insert the node into the map *)
    ignore(rptr.node_map <- (NodeMap.add_exn ~key:board_hash ~data:nid rptr.node_map));
    nid
  | Some node_id -> node_id

let find_or_insert_transition (rptr: repertoire) (from_: int) (to_: int) (move: Parser.move): int =
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

let insert_move_from_position (rptr: repertoire) (board: Board.board) (move: Parser.move): repertoire =
  (* You can only insert move because move does not require position checking *)
  let board_after_move = Board.make_move move board in
  let before_idx = find_or_insert_node rptr board in
  let after_idx = find_or_insert_node rptr board_after_move in
  let transition_id = find_or_insert_transition rptr before_idx after_idx move in
  (* Of course we need to append the transition_id to the before rnode's continuation list *)
  ignore(append_transition_to_node rptr transition_id before_idx);
  rptr

let reindex_line (base: repertoire) (targ: repertoire) (line: line): line =
  (* This is the function that maps a line into new rnode index *)
  match line with {line=int_list;repetition=repetition} ->
    let reindex (integer: int): int =
      NodeMap.find_exn (targ.node_map) (Board.to_fen (List.nth_exn base.nodes integer).board) in
    {line=List.map ~f:reindex int_list;repetition=repetition}

let combine_repertoire (rptr: repertoire) (other: repertoire): repertoire =
  (* This function iterates over all positions and continuations from other and adding all transitions *)
  let merge_transition (rptr: repertoire) (board: Board.board) (tr: transition): unit =
    match tr with {move=move;_} ->
      ignore(insert_move_from_position rptr board move)
    in
  let merge_all_transition_from_node (rptr: repertoire) (tlist: transition list) (node: rnode): unit =
    match node with
    | {board=board;continuations=continuations;_} ->
      begin
        List.iter ~f:(merge_transition rptr board) (List.map ~f:(List.nth_exn tlist) continuations)
      end
    in
  List.iter ~f:(merge_all_transition_from_node rptr other.transitions) other.nodes;
  (* We also need to reindex all the lines in the original repertoire *)
  rptr.lines <- (rptr.lines @ List.map ~f:(reindex_line other rptr) other.lines);
  rptr