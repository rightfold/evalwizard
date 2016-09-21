open Core.Std
module Http = Cohttp_lwt_unix.Client

module Json : sig
  type t = Yojson.Basic.json
  val find : t -> string -> t option
  val path : t -> string list -> t option
end = struct
  type t = Yojson.Basic.json
  let rec path j p =
    match (j, p) with
      | (_, []) -> Some j
      | (`Assoc obj, k :: tl) ->
          begin
            match List.Assoc.find obj k with
            | Some v -> path v tl
            | None -> None
          end
      | _ -> None
  let find j k = path j [k]
end

module Entry : sig
  type t = [
    | `Package of string
    | `Value   of (string * string)
    | `Type    of (string * string)
  ]
  val parse_one : Json.t -> t list
  val parse_all : Json.t -> t list
  val format    : t -> string
end = struct
  type t = [
    | `Package of string
    | `Value   of (string * string)
    | `Type    of (string * string)
  ]

  let parse_one j =
    match (Json.path j ["info"; "type"], Json.find j "package") with
    | (Some (`String "package"), Some (`String package)) ->
        [`Package package]
    | (Some (`String "declaration"), Some (`String package)) ->
        begin match (Json.path j ["info"; "typeOrValue"], Json.path j ["info"; "title"]) with
          | (Some (`String "Value"), Some (`String title)) -> [`Value (package, title)]
          | (Some (`String "Type"), Some (`String title)) -> [`Type (package, title)]
          | _ -> []
        end
    | _ -> []

  let parse_all = function
    | `List js -> List.bind js parse_one
    | _ -> []

  let format = function
    | `Package name -> Printf.sprintf "%-30s (package)" name
    | `Value (package, name) -> Printf.sprintf "%-30s (value, %s)" name package
    | `Type (package, name) -> Printf.sprintf "%-30s (type, %s)" name package
end

let headers = Cohttp.Header.init_with "Accept" "application/json"

let make url = object
  method permission _ () = true
  method execute query () =
    let url = Uri.add_query_param' url ("q", query) in
    let (_, response_body) = Lwt_main.run (Http.get ~headers url) in
    let json_text = Lwt_main.run (Cohttp_lwt_body.to_string response_body) in
    let json = Yojson.Basic.from_string json_text in
    List.take (List.map (Entry.parse_all json) Entry.format) 3 @ [Uri.to_string url];
end
