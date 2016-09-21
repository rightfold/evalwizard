module Http = Cohttp_lwt_unix.Client

let headers = Cohttp.Header.init_with "Accept" "application/json"

let make url = object
  method permission _ () = true
  method execute query () =
    let url = Uri.add_query_param' url ("q", query) in
    let (_, response_body) = Lwt_main.run (Http.get ~headers url) in
    let response_body_text = Lwt_main.run (Cohttp_lwt_body.to_string response_body) in
    print_endline response_body_text;
    []
end
