let server =
  Dream.router [
    Dream.post "/double" (fun request ->
      let%lwt body = Dream.body request in
      Dream.respond (body ^ body));
  ]
  @@ Dream.not_found

let () =
  ignore (Dream.serve server);
  Lwt_main.run (Lwt_unix.sleep 0.1)

let response =
  Lwt_main.run (Hyper.post "http://127.0.0.1:8080/double" "abc")

let () =
  print_endline response
