let server =
  Dream.router [
    Dream.get "/redirect" (fun request ->
      Dream.redirect request "/target");

    Dream.get "/target" (fun _request ->
      Dream.respond "Hello, world!");
  ]
  @@ Dream.not_found

let () =
  ignore (Dream.serve server);
  Lwt_main.run (Lwt_unix.sleep 0.1)

let response =
  Lwt_main.run (Hyper.get ~server "http://127.0.0.1:8080/redirect")

let () =
  print_endline response
