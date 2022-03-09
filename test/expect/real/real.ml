(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



let%expect_test _ =
  ignore (Lwt_main.run (Hyper.get "https://github.com"));
  [%expect {| |}]

let%expect_test _ =
  let url =
    "https://login.microsoftonline.com/consumers/v2.0/.well-known/" ^
    "openid-configuration" in
  ignore (Lwt_main.run (Hyper.get url));
  [%expect {| |}]
