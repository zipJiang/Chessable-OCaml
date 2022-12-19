(* 
  This file should be incrementally built up to have the ability to
  1) build up repertoire
  2) provide repertoire information to the frontend
*)

open Core
open Chessable_ocaml

type message_object = {
  message: string;
}[@@deriving yojson]

(* TODO: You may want to make this dynamic for content selection. *)
let load_prptr_str (filename: string): string =
  In_channel.read_all filename

let run_server (repertoire_filename: string) = 
  Dream.run
  @@ Dream.logger
  @@ Dream.origin_referrer_check
  @@ Dream.router [
      Dream.get "/"
        (fun _ -> 
          repertoire_filename
          |> load_prptr_str
          |> (Dream.json ~headers:["Access-Control-Allow-Origin", "*"]) )
  ]

let combine_pgn_to_repertoire (filename: string) (pgn_name: string): unit =
  (* Running this commend will append pgn to a repertoire file if it exist, if not, will create one *)
  match Sys_unix.file_exists filename with
  | `Yes -> 
    let rptr = Repertoire.load_repertoire_from_file filename in
    let pgn_parse = Parser.parse_pgn_file pgn_name in
    let root = pgn_parse.lines in
    let board = Board.initialize () in
    let start_move = List.nth_exn root.continuation 0 in
    Repertoire.save_repertoire_to_file (Repertoire.merge_move_tree board rptr start_move) filename
  | `No ->
    let rptr = Repertoire.initialize () in
    let pgn_parse = Parser.parse_pgn_file pgn_name in
    let root = pgn_parse.lines in
    let board = Board.initialize () in
    let start_move = List.nth_exn root.continuation 0 in
    Repertoire.save_repertoire_to_file (Repertoire.merge_move_tree board rptr start_move) filename
  | `Unknown ->
    failwith "Should not handle unknown case!"

(*We start a commandlind interface for this*)
let () =
    Command.basic
      ~summary:"A locally working version of Chessable."
      [%map_open.Command
          let task = anon ("TASK" %: string)
          and filename = anon ("REPERTOIRE-FILE" %: string)
          and pgn_name = flag "--pgn" (optional string) ~doc:"Target pgn file to read for generating the target resource."
        in
        fun () -> 
          match task with
          | "input" -> 
            (* Try to input new pgn fdile to repertoire *)
            begin
              match pgn_name with
              | None -> failwith "Need to specify pgn file name to combine."
              | Some pgn_name ->
                combine_pgn_to_repertoire filename pgn_name
            end
          | "serve" -> 
            run_server filename
          | _ -> failwith "Not valid task."
      ]
    |> Command_unix.run