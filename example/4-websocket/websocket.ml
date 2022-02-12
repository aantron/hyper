let server =
  Dream.router [
    Dream.get "/socket" (fun _request ->
      Dream.websocket (fun websocket ->
        match%lwt Dream.receive websocket with
        | Some "Hello" ->
          let%lwt () = Dream.send websocket "world!" in
          Dream.close_websocket websocket
        | _ ->
          Dream.close_websocket websocket));
  ]

let () =
  ignore (Dream.serve server);
  Lwt_main.run (Lwt_unix.sleep 0.1)

let response =
  Lwt_main.run begin
    let%lwt websocket = Hyper.websocket "ws://127.0.0.1:8080/socket" in
    let%lwt () = Hyper.send websocket "Hello" in
    Hyper.receive websocket
  end

let () =
  print_endline (Option.get response)
