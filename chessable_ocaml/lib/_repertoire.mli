(* 
    This is the declaration of a repertoire class, where
    we need to track the moves that reaches this position,
    possibly moves that can be made at this position and
    the actual position at this moment.
 *)
 type rnode
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