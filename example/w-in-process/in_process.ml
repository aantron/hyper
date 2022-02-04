let server =
  Dream.logger
  @@ Dream.router [
    Dream.get "/:text" (fun request ->
      Dream.respond (Dream.param request "text"));
  ]
  @@ Dream.not_found

let proxy =
  Dream.logger
  @@ (fun request -> ignore request; raise Exit)

let client () =
  let%lwt body = Hyper.get ~server:proxy "http://127.0.0.1:8080/abc" in
  print_endline body;
  Lwt.return ()

(* let () =
  Lwt_main.run (client ()) *)

let () =
  Lwt.async (fun () ->
    Lwt_unix.sleep 1.;%lwt
    client ())

let () =
  Dream.run server
