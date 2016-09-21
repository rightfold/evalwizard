open Core.Std
open Evalwizard
module Irc = Irc_client_unix

let commands =
  let help = Help.make "http://foldr.nl/Projects/Evalwizard" in
  let pursuit = Pursuit.make (Uri.of_string "https://pursuit.purescript.org/search") in
  [ ("h",       help)
  ; ("help",    help)
  ; ("p",       pursuit)
  ; ("pursuit", pursuit)
  ]

let handle_message connection msg () =
  let open Irc_message in
  match msg.command with
  | PRIVMSG (target, text) ->
      begin
        match Evalwizard_irc.parse text with
        | Some (opcode, argument) ->
            (* TODO: handle exceptions *)
            (* TODO: ignore own messages *)
            let command = Command.find opcode commands in
            (* TODO: check permission *)
            let result = command#execute argument () in
            (* TODO: truncate result *)
            List.iter result (fun line ->
              (* TODO: truncate line *)
              (* TODO: replace control characters by symbolic equivalents *)
              Irc.send connection (privmsg target line)
            )
        | None -> ()
      end
  | _ -> ()

let () =
  let addr = (Caml.Unix.gethostbyname "irc.freenode.net").h_addr_list.(0) in
  let port = 6667 in
  let nick = "evalwizard" in
  let connection = Irc.connect ~addr ~port ~nick () in
  Irc.send_join connection "#evalwizard";
  Irc.listen connection (fun _ -> function
    | `Ok msg -> handle_message connection msg ()
    | `Error msg -> print_endline msg
  )
