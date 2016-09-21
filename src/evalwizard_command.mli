type t =
  < permission : string -> unit -> bool
  ; execute    : string -> unit -> string list
  >

val find : string -> (string * t) list -> t
