(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



let () =
  Test_expect.Helpers.last_port := 8079

let test request =
  Hyper__http.Connect.no_pool ~transport:`HTTP1 request

let%expect_test _ =
  Test_expect_http.Suite.http test;
  [%expect {|
    test: basic
    foo
    test: post
    POST
    test: custom method
    FOO
    test: target
    /target
    test: query
    /target?foo=bar
    test: anchor
    /target
    test: headers
    Foo: bar
    Baz: quux
    Transfer-Encoding: chunked
    test: response
    599
    Foo: bar
    Content-Length: 3
    test: request body
    foobar
    test: request body chunked
    foobar
    test: request body stream
    foobar
    test: request flush, ping, pong
    foobar
    test: auto-flush
    32768
    test: early exception
    Exit
    test: late exception
    Exit
    test: double body
    foobar
    foobar
    test: request body closed by peer

    test: response body closed by client
    foobar
    test: response body aborted
    Exit
    test: read after eof
    foobar
    None |}]



(* Malformed response. *)
let%expect_test _ =
  begin
    print_endline "test: malformed response";
    let port = Test_expect.Helpers.next_port () in
    Test_expect.Helpers.with_tcp_responder port "" @@ fun () ->
    let request = Hyper.request "http://127.0.0.1/" in
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
    malformed response: unexpected eof |}]

(* Invalid Content-Length. *)
let%expect_test _ =
  begin
    print_endline "test: invalid content length";
    let port = Test_expect.Helpers.next_port () in
    Test_expect.Helpers.with_tcp_responder port
      "HTTP/1.1 200 OK\r\nContent-Length: -1\r\n\r\n" @@ fun () ->
    let request = Hyper.request "http://127.0.0.1/" in
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
