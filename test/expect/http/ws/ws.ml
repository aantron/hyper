(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



let () =
  Test_expect.Helpers.last_port := 8109

let test request =
  Hyper__http.Connect.no_pool ~transport:`WS request

let%expect_test _ =
  Test_expect_http.Suite.ws test;
  [%expect {|
    test: basic
    Switching Protocols
    bar
    test: non-WebSocket response
    See Other
    /socket
    foobar
    test: non-WebSocket: client closes response stream
    foobar
    test: non-WebSocket: client aborts response stream
    foobar
    test: non-WebSocket: read after end of stream
    foobar
    None |}]

(* Malformed response. *)
(* TODO Hangs; debug. *)
(* let%expect_test _ =
  begin
    print_endline "test: malformed response";
    let port = Test_expect.Helpers.next_port () in
    Test_expect.Helpers.with_tcp_responder port "" @@ fun () ->
    let request = Hyper.request "ws://127.0.0.1/socket" in
    Test_expect.Helpers.set_port request port;
    match%lwt test request with
    | exception Failure message ->
      print_endline message;
      Lwt.return_unit
    | _response ->
      print_endline "unexpected response";
      Lwt.return_unit
  end;
  [%expect {|
    test: malformed response
    malformed response: unexpected eof |}] *)

(* Invalid Content-Length. *)
let%expect_test _ =
  begin
    print_endline "test: invalid content length";
    let port = Test_expect.Helpers.next_port () in
    Test_expect.Helpers.with_tcp_responder port
      "HTTP/1.1 200 OK\r\nContent-Length: -1\r\n\r\n" @@ fun () ->
    let request = Hyper.request "ws://127.0.0.1/socket" in
    Test_expect.Helpers.set_port request port;
    match%lwt test request with
    | exception Failure message ->
      print_endline message;
      Lwt.return_unit
    | _response ->
      print_endline "unexpected response";
      Lwt.return_unit
  end;
  [%expect {|
    test: invalid content length
    invalid response body length |}]
