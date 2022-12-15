(* 
  This file should be incrementally built up to have the ability to
  1) build up repertoire
  2) provide repertoire information to the frontend
*)

type message_object = {
  message: string;
}[@@deriving yojson]

let get_message_object (result: 'a Ppx_deriving_yojson_runtime.error_or): 'a =
  match result with
  | Error msg -> failwith msg
  | Ok some -> some

(* TODO: You may want to make this dynamic for content selection. *)
let repertoire_str: string = "/home/zipjiang/Documents/proj/Chessable-OCaml/data/debug_repertoire.json"

let load_prptr_str (filename: string): string =
  let in_channel = In_channel.open_text filename in
  In_channel.input_all in_channel

let () = 
  Dream.run
  @@ Dream.logger
  @@ Dream.origin_referrer_check
  @@ Dream.router [
      Dream.get "/"
        (fun _ -> 
          repertoire_str
          |> load_prptr_str
          |> Dream.json )
  ]