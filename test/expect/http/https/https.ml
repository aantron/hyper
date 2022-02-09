(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



let () =
  Test_expect.Helpers.last_port := 8089

let test request =
  Hyper__http.Connect.no_pool ~transport:`HTTPS request

let%expect_test _ =
  Test_expect_http.Suite.http ~https:true test;
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
    Transfer-Encoding: chunked
    Foo: bar
    Baz: quux
    test: response
    599
    Foo: bar
    Transfer-Encoding: chunked
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

    test: response body aborted
    Exit
    test: read after eof
    foobar
    None |}]
