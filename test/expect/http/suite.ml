(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



module Message = Dream_pure.Message
module Status = Dream_pure.Status
module Stream = Dream_pure.Stream



(* TODO What happens when the client sends a request that the server never even
   tries to read, and never closes? Maybe the connection concept is useful,
   after all. *)
let server =
  Dream.router [
    Dream.get "/basic" (fun _request ->
      Dream.respond "foo");

    Dream.any "/method" (fun request ->
      Dream.method_ request
      |> Dream.method_to_string
      |> Dream.respond);

    Dream.get "/target" (fun request ->
      Dream.target request
      |> Dream.respond);

    Dream.get "/headers" (fun request ->
      Dream.all_headers request
      |> List.map (fun (name, value) -> Printf.sprintf "%s: %s" name value)
      |> String.concat "\n"
      |> Dream.respond);

    Dream.get "/weird-response" (fun _request ->
      Dream.respond ~code:599 ~headers:["Foo", "bar"] "");

    Dream.post "/echo" (fun request ->
      let%lwt body = Dream.body request in
      Dream.respond body);

    Dream.post "/stream" (fun request ->
      Dream.stream (fun stream ->
        let%lwt () = Dream.flush stream in
        let rec loop () =
          match%lwt Dream.read (Dream.server_stream request) with
          | Some chunk ->
            let%lwt () = Dream.write stream chunk in
            loop ()
          | None ->
            Dream.close stream
        in
        loop ()));

    Dream.post "/close" (fun request ->
      let%lwt () = Dream.close (Dream.server_stream request) in
      Dream.respond "");

    Dream.get "/socket" (fun _request ->
      Dream.websocket (fun response ->
        match%lwt Dream.read response with
        | Some "foo" ->
          let%lwt () = Dream.write response "bar" in
          Dream.close response
        | _ ->
          Dream.close response));

    Dream.get "/socket-redirect" (fun request ->
      let%lwt response = Dream.redirect request "/socket" in
      Dream.set_body response "foobar";
      Lwt.return response);
  ]
  @@ Dream.not_found

let start https nohttp test =
  if not nohttp then begin
    let port = Test_expect.Helpers.next_port () in
    let stop, stop_server = Lwt.wait () in
    let stopped = Dream.serve ~port ~https ~stop server in
    let test request =
      Test_expect.Helpers.set_port request port;
      test request
    in
    let () = Lwt_main.run (Lwt_unix.sleep 0.1) in
    stop_server, stopped, test
  end
  else begin
    let stopped, stop_server = Lwt.wait () in
    stop_server, stopped, test
  end



let http ?(https = false) ?(nohttp = false) test =
  let stop_server, stopped, test = start https nohttp test in



  (* Basic request-response. *)
  let () =
    print_endline "test: basic";
    Lwt_main.run begin
      let request = Hyper.request "http://127.0.0.1/basic" in
      let%lwt response = test request in
      let%lwt response = Hyper.body response in
      print_endline response;
      Lwt.return_unit
    end
  in



  (* Various methods. *)
  let () =
    print_endline "test: post";
    Lwt_main.run begin
      let request = Hyper.request ~method_:`POST "http://127.0.0.1/method" in
      let%lwt response = test request in
      let%lwt response = Hyper.body response in
      print_endline response;
      Lwt.return_unit
    end
  in

  let () =
    print_endline "test: custom method";
    Lwt_main.run begin
      let method_ = `Method "FOO" in
      let request = Hyper.request ~method_ "http://127.0.0.1/method" in
      let%lwt response = test request in
      let%lwt response = Hyper.body response in
      print_endline response;
      Lwt.return_unit
    end
  in



  (* Targets. *)
  let () =
    print_endline "test: target";
    Lwt_main.run begin
      let request = Hyper.request "http://127.0.0.1/target" in
      let%lwt response = test request in
      let%lwt response = Hyper.body response in
      print_endline response;
      Lwt.return_unit
    end
  in

  let () =
    print_endline "test: query";
    Lwt_main.run begin
      let request = Hyper.request "http://127.0.0.1/target?foo=bar" in
      let%lwt response = test request in
      let%lwt response = Hyper.body response in
      print_endline response;
      Lwt.return_unit
    end
  in

  let () =
    print_endline "test: anchor";
    Lwt_main.run begin
      let request = Hyper.request "http://127.0.0.1/target#foo" in
      let%lwt response = test request in
      let%lwt response = Hyper.body response in
      print_endline response;
      Lwt.return_unit
    end
  in



  (* Headers. *)
  let () =
    print_endline "test: headers";
    Lwt_main.run begin
      let headers = ["Foo", "bar"; "Baz", "quux"] in
      let request = Hyper.request ~headers "http://127.0.0.1/headers" in
      let%lwt response = test request in
      let%lwt response = Hyper.body response in
      print_endline response;
      Lwt.return_unit
    end
  in



  (* Response fields. *)
  let () =
    print_endline "test: response";
    Lwt_main.run begin
      let request = Hyper.request "http://127.0.0.1/weird-response" in
      let%lwt response = test request in
      Printf.printf "%i\n" (Message.status response |> Status.status_to_int);
      Message.all_headers response
      |> List.iter (fun (name, value) -> Printf.printf "%s: %s\n" name value);
      flush stdout;
      Lwt.return_unit
    end
  in



  (* Request bodies. *)
  (* TODO The body length and/or TE headers should be placed automatically. *)
  let () =
    print_endline "test: request body";
    Lwt_main.run begin
      let body = Stream.string "foobar" in
      (* let headers = ["Content-Length", "6"] in *)
      let request =
        Hyper.request ~method_:`POST ~body "http://127.0.0.1/echo" in
      let%lwt response = test request in
      let%lwt response = Hyper.body response in
      print_endline response;
      Lwt.return_unit
    end
  in

  let () =
    print_endline "test: request body chunked";
    Lwt_main.run begin
      let body = Stream.string "foobar" in
      (* let headers = ["Transfer-Encoding", "chunked"] in *)
      let request =
        Hyper.request ~method_:`POST ~body "http://127.0.0.1/echo" in
      let%lwt response = test request in
      let%lwt response = Hyper.body response in
      print_endline response;
      Lwt.return_unit
    end
  in



  (* Streaming body. *)
  (* TODO Need a higher-level pipe API, probably. *)
  let () =
    print_endline "test: request body stream";
    Lwt_main.run begin
      let reader, writer = Stream.pipe () in
      let client_stream = Stream.stream Stream.no_reader writer in
      let server_stream = Stream.stream reader Stream.no_writer in
      (* let headers = ["Transfer-Encoding", "chunked"] in *)
      let request =
        Message.request
          ~method_:`POST ~target:"http://127.0.0.1/stream"
          client_stream server_stream
      in
      let write_promise = Message.write client_stream "foo" in
      let%lwt response = test request in
      let body_promise = Hyper.body response in
      let%lwt () = write_promise in
      let%lwt () = Message.write client_stream "bar" in
      let%lwt () = Message.close client_stream in
      let%lwt response = body_promise in
      print_endline response;
      Lwt.return_unit
    end
  in

  (* TODO This test is broken in nohttp; it pong should probably just be
     silently ignored. Or somehow dropped if the stream is not a WebSocket
     stream. *)
  (* Writing continues across pings, pongs, and flushes. *)
  if not nohttp then begin
    print_endline "test: request flush, ping, pong";
    Lwt_main.run begin
      let reader, writer = Stream.pipe () in
      let client_stream = Stream.stream Stream.no_reader writer in
      let server_stream = Stream.stream reader Stream.no_writer in
      (* let headers = ["Transfer-Encoding", "chunked"] in *)
      let request =
        Message.request
          ~method_:`POST ~target:"http://127.0.0.1/stream"
          client_stream server_stream
      in
      let%lwt response = test request in
      let body_promise = Hyper.body response in
      let%lwt () = Message.write client_stream "foo" in
      let%lwt () = Message.flush client_stream in
      let ping_pong_promise, ping_pong_done = Lwt.wait () in
      Stream.ping client_stream Bigstringaf.empty 0 0
        ~close:(fun _ -> Lwt.wakeup_later_exn ping_pong_done End_of_file)
        ~exn:(Lwt.wakeup_later_exn ping_pong_done)
        (fun () ->
          Stream.pong client_stream Bigstringaf.empty 0 0
            ~close:(fun _ -> Lwt.wakeup_later_exn ping_pong_done End_of_file)
            ~exn:(Lwt.wakeup_later_exn ping_pong_done)
            (Lwt.wakeup_later ping_pong_done));
      let%lwt () = ping_pong_promise in
      let%lwt () = Message.write client_stream "bar" in
      let%lwt () = Message.close client_stream in
      let%lwt response = body_promise in
      print_endline response;
      Lwt.return_unit
    end
  end;

  (* Writing continues across an auto-flush (which is done for flow control). *)
  let () =
    print_endline "test: auto-flush";
    Lwt_main.run begin
      let reader, writer = Stream.pipe () in
      let client_stream = Stream.stream Stream.no_reader writer in
      let server_stream = Stream.stream reader Stream.no_writer in
      (* let headers = ["Transfer-Encoding", "chunked"] in *)
      let request =
        Message.request
          ~method_:`POST ~target:"http://127.0.0.1/stream"
          client_stream server_stream
      in
      let%lwt response = test request in
      let body_promise = Hyper.body response in
      let chunk_1 = String.make 16384 'a' in
      let chunk_2 = String.make 16384 'b' in
      let%lwt () = Message.write client_stream chunk_1 in
      let%lwt () = Message.write client_stream chunk_2 in
      let%lwt () = Message.close client_stream in
      let%lwt response = body_promise in
      Printf.printf "%i\n%!" (String.length response);
      Lwt.return_unit
    end
  in



  (* Early exception, pushed into the request body stream likely before the
     response has been received by the client. This should cause rejection of
     the response promise. Presumably, http/af knows how to clean up its own
     connection at that point. *)
  let () =
    print_endline "test: early exception";
    Lwt_main.run begin
      let body = Stream.string "foobar" in
      Stream.abort body Exit;
      (* let headers = ["Transfer-Encoding", "chunked"] in *)
      let request =
        Hyper.request ~method_:`POST ~body "http://127.0.0.1/echo" in
      match%lwt test request with
      | exception Exit ->
        print_endline "Exit";
        Lwt.return_unit
      | _response ->
        print_endline "unexpected response";
        Lwt.return_unit
    end
  in

  (* Late exception, pushed into the request body stream after the response has
     been received by the client. This should trigger an exception callback in
     the body-reading stream. http/af should be able to clean up its part of the
     connection.

     This test fails with a stack trace on nohttp. It forces the server to
     receive the Exit exception that the client pushes in. The server then
     reports it to Lwt.async_exception_hook. On an actual server (i.e., not
     nohttp), Dream would capture and log that. However, with nohttp, the
     server is directly tied into the client and there is no special error
     handler. So, the exception simply kills the testing process. The test
     could be "saved" by saving !Lwt.async_exception_hook across the test's
     body, but it seems better to avoid it for now. *)
  (* TODO Use higher-level functions in this test once a streaming request can
     be created. *)
  if not nohttp then begin
    print_endline "test: late exception";
    Lwt_main.run begin
      let reader, writer = Stream.pipe () in
      let client_stream = Stream.stream Stream.no_reader writer in
      let server_stream = Stream.stream reader Stream.no_writer in
      (* let headers = ["Transfer-Encoding", "chunked"] in *)
      let request =
        Message.request
          ~method_:`POST ~target:"http://127.0.0.1/stream"
          client_stream server_stream
      in
      let%lwt response = test request in
      Stream.abort client_stream Exit;
      match%lwt Hyper.body response with
      | exception Exit ->
        print_endline "Exit";
        Lwt.return_unit
      | _response ->
        print_endline "unexpected data";
        Lwt.return_unit
    end
  end;



  (* Double read with body caching. *)
  let () =
    print_endline "test: double body";
    Lwt_main.run begin
      let body = Stream.string "foobar" in
      let headers = ["Content-Length", "6"] in
      let request =
        Hyper.request ~method_:`POST ~headers ~body "http://127.0.0.1/echo" in
      let%lwt response = test request in
      let%lwt response' = Hyper.body response in
      print_endline response';
      let%lwt response' = Hyper.body response in
      print_endline response';
      Lwt.return_unit
    end
  in

  (* Request body stream closed by peer. *)
  (* TODO This test is essentially meaningless, because there is no way to
     signal body reading close within HTTP, while the server currently has no
     way of forcing the surrounding socket connection to be closed. Or, this
     test is partially meaningful, but a test that expects the connection to be
     closed during body writing is also needed. *)
  let () =
    print_endline "test: request body closed by peer";
    Lwt_main.run begin
      let body = Stream.string "foobar" in
      (* let headers = ["Transfer-Encoding", "chunked"] in *)
      let request =
        Hyper.request ~method_:`POST ~body "http://127.0.0.1/close" in
      let%lwt response = test request in
      let%lwt response = Hyper.body response in
      print_endline response;
      Lwt.return_unit
    end
  in

  (* Client closes response body stream abruptly. *)
  (* TODO It might also be good to test whether the underlying connection is
     closed in this test. *)
  (* TODO Interestingly, immediate close of the http/af body reader still allows
     the received body to be read. Perhaps a test with large data chunks can
     show other behavior. *)
  let () =
    print_endline "test: response body closed by client";
    Lwt_main.run begin
      let body = Stream.string "foobar" in
      (* let headers = ["Transfer-Encoding", "chunked"] in *)
      let request =
        Hyper.request ~method_:`POST ~body "http://127.0.0.1/echo" in
      let%lwt response = test request in
      let stream = Message.client_stream response in
      Stream.close stream 1000;
      let%lwt response = Hyper.body response in
      print_endline response;
      Lwt.return_unit
    end
  in

  (* Client aborts response stream with an exception. *)
  let () =
    print_endline "test: response body aborted";
    Lwt_main.run begin
      let body = Stream.string "foobar" in
      (* let headers = ["Transfer-Encoding", "chunked"] in *)
      let request =
        Hyper.request ~method_:`POST ~body "http://127.0.0.1/echo" in
      let%lwt response = test request in
      let stream = Message.client_stream response in
      Stream.abort stream Exit;
      match%lwt Hyper.body response with
      | exception Exit ->
        print_endline "Exit";
        Lwt.return_unit
      | _response ->
        print_endline "unexpected response";
        Lwt.return_unit
    end
  in

  (* Read after end of response stream. *)
  let () =
    print_endline "test: read after eof";
    Lwt_main.run begin
      let body = Stream.string "foobar" in
      let request =
        Hyper.request ~method_:`POST ~body "http://127.0.0.1/echo" in
      let%lwt response = test request in
      let%lwt response' = Hyper.body response in
      print_endline response';
      match%lwt Message.read (Message.client_stream response) with
      | None ->
        print_endline "None";
        Lwt.return_unit
      | Some _chunk ->
        print_endline "unexpected chunk";
        Lwt.return_unit
    end
  in



  let () =
    Lwt.wakeup_later stop_server ();
    Lwt_main.run stopped
  in



  ()



let ws ?(https = false) ?(nohttp = false) test =
  let stop_server, stopped, test = start https nohttp test in



  (* Basic. *)
  let () =
    print_endline "test: basic";
    Lwt_main.run begin
      let request = Hyper.request "ws://127.0.0.1/socket" in
      let%lwt response = test request in
      Message.status response |> Status.status_to_string |> print_endline;
      match Message.get_websocket response with
      | None ->
        print_endline "not a WebSocket";
        Lwt.return_unit
      | Some (client_stream, _server_stream) ->
        let%lwt () = Message.write client_stream "foo" in
        let%lwt message = Message.read client_stream in
        let%lwt () = Message.close client_stream in
        begin match message with
        | Some message -> print_endline message
        | None -> print_endline "None"
        end;
        Lwt.return_unit
    end
  in



  (* Non-WebSocket response. *)
  let () =
    print_endline "test: non-WebSocket response";
    Lwt_main.run begin
      let request = Hyper.request "ws://127.0.0.1/socket-redirect" in
      let%lwt response = test request in
      Message.status response |> Status.status_to_string |> print_endline;
      match Message.get_websocket response with
      | Some _ ->
        print_endline "got a WebSocket";
        Lwt.return_unit
      | None ->
        match Message.header response "Location" with
        | None ->
          print_endline "no Location: header";
          Lwt.return_unit
        | Some location ->
          print_endline location;
          let%lwt body = Hyper.body response in
          print_endline body;
          Lwt.return_unit
    end
  in

  (* Non-WebSocket: client closes response stream. *)
  let () =
    print_endline "test: non-WebSocket: client closes response stream";
    Lwt_main.run begin
      let request = Hyper.request "ws://127.0.0.1/socket-redirect" in
      let%lwt response = test request in
      let stream = Message.client_stream response in
      let%lwt () = Message.close stream in
      let%lwt body = Hyper.body response in
      print_endline body;
      Lwt.return_unit
    end
  in

  (* Non-WebSocket: client aborts response stream. *)
  (* TODO This is equivalent to closing without an exception, due to
     https://github.com/anmonteiro/websocketaf/issues/40. *)
  let () =
    print_endline "test: non-WebSocket: client aborts response stream";
    Lwt_main.run begin
      let request = Hyper.request "ws://127.0.0.1/socket-redirect" in
      let%lwt response = test request in
      let stream = Message.client_stream response in
      Stream.abort stream Exit;
      let%lwt body = Hyper.body response in
      print_endline body;
      Lwt.return_unit
    end
  in

  (* Non-WebSocket: read after end of stream. *)
  let () =
    print_endline "test: non-WebSocket: read after end of stream";
    Lwt_main.run begin
      let request = Hyper.request "ws://127.0.0.1/socket-redirect" in
      let%lwt response = test request in
      let%lwt body = Hyper.body response in
      print_endline body;
      match%lwt Message.read (Message.client_stream response) with
      | None ->
        print_endline "None";
        Lwt.return_unit
      | Some _chunk ->
        print_endline "unexpected chunk";
        Lwt.return_unit
    end
  in



  let () =
    Lwt.wakeup_later stop_server ();
    Lwt_main.run stopped
  in

  ()
