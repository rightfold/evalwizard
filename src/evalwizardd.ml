open Core.Std
open Evalwizard

let commands =
  let help = Help.make "http://foldr.nl/Projects/Evalwizard" in
  [ ("h",    help)
  ; ("help", help)
  ]

let () =
  while true do
    let line = read_line () in
    let (opcode, argument) =
      match String.lsplit2 line ' ' with
      | Some (opcode, argument) -> (opcode, argument)
      | None -> (line, "")
    in
    let command = Command.find opcode commands in
    let result = command#execute argument () in
    List.iter result (fun line -> print_string (line ^ "\n"))
  done
