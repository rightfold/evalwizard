open Core.Std

type t =
  < permission : string -> unit -> bool
  ; execute    : string -> unit -> string list
  >

let not_found opcode = object
  method permission _ () = true
  method execute _ () = ["no such command: '" ^ opcode ^ "'"]
end

let find opcode commands =
  match List.Assoc.find commands opcode with
  | Some command -> command
  | None -> not_found opcode
