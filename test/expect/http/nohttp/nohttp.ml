(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



let test request =
  Test_expect_http.Suite.server request

let%expect_test _ =
  Test_expect_http.Suite.http ~nohttp:true test;
  [%expect {|
    test: basic
    foo
    test: post
    POST
    test: custom method
    FOO
    test: target
    http://127.0.0.1/target
    test: query
    http://127.0.0.1/target?foo=bar
    test: anchor
    http://127.0.0.1/target#foo
    test: headers
    Foo: bar
    Baz: quux
    test: response
    599
    Foo: bar
    test: request body
    foobar
    test: request body chunked
    foobar
    test: request body stream
    foobar
    test: auto-flush
    32768
    test: early exception
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
