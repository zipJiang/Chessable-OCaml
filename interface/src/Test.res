// Declare type for the json data of repertoire
type repetition = {
    level: int,
    last_seen: int
}

type piece = array<string>

type side = array<string>

type square = {
    col: string,
    rol: int
}

type mv = {
    piece: piece,
    target: option<square>,
    // Notice that it seems that in javascript we don't have char type, so we are using string for consistency
    start_spec: option<string>,   
    is_check: bool,
    is_take: bool,
    is_mate: bool,
    is_castle_k: bool,
    is_castle_q: bool,
    remark: option<string>,
    promote: option<piece>
}

type move = {
    side: side,
    move: mv,
    turn_id: int,
    comment: option<string>
}

type transition = {
    transition_id: int,
    from_: int,
    to_: int,
    move: move
}

type line = {
    from_node: int,
    repetition: repetition,
    line: array<int>
}

type rnode = {
    node_id: int,
    board_hash: string,
    continuations: array<int>
}

type repertoire = {
    lines: array<line>,
    nodes: array<rnode>,
    transitions: array<transition>
}


 
@react.component
let make = () => {
    <div> {React.string("Hello World!")} </div>
}