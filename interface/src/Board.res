// This should implement a react component that corresponds to a chessboard

module Chessboard = {
    @module("react-chessboard") @react.component external make: (~position:string, ~onPieceDrop:'t) => React.element = "Chessboard"
}

let move_to_text = (move) => {
  if move["move"]["is_castle_k"] {
    "O-O"
  }
  else if move["move"]["is_castle_q"] {
    "O-O-O"
  }
  else {
    let piece_text =
    switch move["move"]["piece"] {
    | ["Pawn"] => ""
    | ["Knight"] => "N"
    | ["Bishop"] => "B"
    | ["King"] => "K"
    | ["Queen"] => "Q"
    | ["Rook"] => "R"
    }
    let starting_annotation_text = {
        let c = move["move"]["start_spec"]
        if c === Js.Null.empty
            {""}
        else {Js.Null.getExn(c)}
    }
    let target_square_text =
    switch move["move"]["target"] {
    | None => ""
    | Some(sqr) => sqr["col"] ++ Belt.Int.toString(sqr["row"])
    }
    piece_text ++ starting_annotation_text ++ target_square_text
  }
}

module Chess = {
    type t
    type move_return
    type sloppy_dict = {sloppy: bool}
    type pos_move_dict = {from: string, to: string}
    type piece_dict = {"type": string, "color": string}
    @new @module("chess.js") external chess: (string) => t = "Chess"
    @send external ascii: t => string = "ascii"
    @send external fen: t => string = "fen"
    @send external move: (t, string, sloppy_dict) => move_return = "move"
    @send external move2: (t, pos_move_dict) => Js.Null.t<move_return> = "move"
    @send external get: (t, string) => Js.Null.t<piece_dict> = "get"
}

@react.component
let make = (~lines, ~nodes, ~transitions, ~line_select, ~onClick) => {
    /* TODO: implement some state options for interaction
    */

    let line = lines[line_select]
    let from_node_idx = line["from_node"]
    let find_nidx = (node) => {node["node_id"] == from_node_idx}

    let begin_node = {
        open Js.Array2
        (nodes -> filter(find_nidx))[0]
    }

    let (game, setGame) = React.useState(_ => begin_node["board_hash"])
    let (lid, setLid) = React.useState(_ => 0)
    let (hasBeenMistaken, setHasBeenMistaken) = React.useState(_ => false)
    let (cStatus, setCStatus) = React.useState(_=>"board-undefine")

    let find_tidx = (transition) => {transition["transition_id"] == line["line"][lid]}

    Js.Console.log(line)
    Js.Console.log(("LINE_ID", lid))

    let onDrop = (sourceSquare, targetSquare) => {
        // If the move is a valid move we will make that move

        let gameState = Chess.chess(game)

        let trans = {
            open Js.Array2
            (transitions -> filter(find_tidx))[0]
        } 

        // First check if the move is the correct move
        let move_dict: Chess.pos_move_dict = {from: sourceSquare, to: targetSquare}

        // Check whether the move dict matches the trans
        let mp = Chess.get(gameState, sourceSquare)
        let mp_text = if mp === Js.Null.empty {
            ""
        } else {
            let pt = Js.Null.getExn(mp)["type"]
            if pt == "p" {""} else {Js.String.toUpperCase(pt)}
        }

        let c = Js.String.slice(~from=0, ~to_=1, sourceSquare)
        let r = Js.String.slice(~from=1, ~to_=2, sourceSquare)

        Js.Console.log(trans["move"])

        if (mp_text ++ c ++ targetSquare == move_to_text(trans["move"])) || (mp_text ++ r ++ targetSquare == move_to_text(trans["move"])) || (mp_text ++ targetSquare == move_to_text(trans["move"])) {
            let move_result = Chess.move2(gameState, move_dict)
            setGame(_=>Chess.fen(gameState))
            // Also need to set state for the current transition
            setLid(lid=>lid + 1)
            setCStatus(_=>"board-correct")
        }
        else {
            setCStatus(_=>"board-error")
            setHasBeenMistaken(_=>true)
        }
    }

    if lid < Js.Array2.length(line["line"]) {

        let trans = {
            open Js.Array2
            (transitions -> filter(find_tidx))[0]
        } 
        let comment = if trans["move"]["comment"] === Js.Null.empty {""} else {Js.Null.getExn(trans["move"]["comment"])}

        <div>
        <div className={"board-container " ++ cStatus}>
            <Chessboard position={game} onPieceDrop={onDrop}></Chessboard>
        </div>
        <div className="comment">
            {React.string(comment)}
        </div>
        </div>

    }
    else {
        // Should generate a next button
        <div>
        <button className={"button-3"} onClick=onClick>{React.string("Next")}</button>
        </div>
    }
}