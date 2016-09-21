open Core.Std

let parse text =
  if String.is_prefix text ~prefix:"%"
    then
      let text = String.drop_prefix text 1 in
      match String.lsplit2 text ' ' with
      | Some (opcode, argument) -> Some (opcode, argument)
      | None -> Some (text, "")
    else None
