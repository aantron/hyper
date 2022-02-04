let server =
  Dream.router [
    Dream.get "/socket" (fun _request ->
      Dream.websocket (fun response ->
        match%lwt Dream.read response with
        | Some "Hello" ->
          let%lwt () = Dream.write response "world!" in
          Dream.close response
        | _ ->
          Dream.close response));
  ]
  @@ Dream.not_found

let () =
  ignore (Dream.serve server);
  Lwt_main.run (Lwt_unix.sleep 0.1)

let response =
  Lwt_main.run begin
    let%lwt response = Hyper.websocket "ws://127.0.0.1:8080/socket" in
    let%lwt () = Hyper.write response "Hello" in
    Hyper.read response
  end

let () =
  print_endline (Option.get response)
