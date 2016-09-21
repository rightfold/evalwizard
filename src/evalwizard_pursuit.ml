open Core.Std
module Http = Cohttp_lwt_unix.Client

let headers = Cohttp.Header.init_with "Accept" "application/json"

type entry = {name : string}

exception Bad_response

let parse json_text =
  let parse_entry = function
    | `Assoc obj ->
        begin
          match List.Assoc.find obj "info" with
          | Some (`Assoc info) ->
              begin
                match List.Assoc.find info "title" with
                | Some (`String title) -> {name = title}
                | _ -> raise Bad_response
              end
          | _ -> raise Bad_response
        end
    | _ -> raise Bad_response
  in
  match Yojson.Basic.from_string json_text with
  | `List entries -> List.map entries parse_entry
  | _ -> raise Bad_response

let format entry =
  entry.name

let make url = object
  method permission _ () = true
  method execute query () =
    let url = Uri.add_query_param' url ("q", query) in
    let (_, response_body) = Lwt_main.run (Http.get ~headers url) in
    let json_text = Lwt_main.run (Cohttp_lwt_body.to_string response_body) in
    List.take (List.map (parse json_text) format) 3 @ [Uri.to_string url];
end
