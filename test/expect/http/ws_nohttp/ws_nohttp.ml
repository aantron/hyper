(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



let test request =
  Test_expect_http.Suite.server request

let%expect_test _ =
  Test_expect_http.Suite.ws ~nohttp:true test;
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
    unexpected chunk |}]
