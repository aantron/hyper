(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



type 'a promise = 'a Dream_pure.Message.promise
type request = Dream_pure.Message.request
type response = Dream_pure.Message.response

(* val send : request -> response promise *)

val no_pool :
  ?transport:[ `HTTP1 | `HTTPS | `HTTP2 | `WS ] -> request -> response promise
(* Used for testing. *)
