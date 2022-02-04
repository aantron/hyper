(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



type connection
type 'a promise = 'a Dream_pure.Message.promise
type request = Dream_pure.Message.request
type response = Dream_pure.Message.response

val close : connection -> unit promise

val http1_cleartext_tcp : string -> connection promise
(* val alpn_https_tcp : string -> connection promise *)
(* TODO This signature is basically useless in the long term. It should give
   some kind of scheme, host, port tuple instead of an opaque function for
   establishing a connection. *)
val ws_cleartext_tcp : string -> connection promise
(* TODO Due to the client API, WebSockets don't really fit into the connection/
   handler scheme. Should still adapt them for uniform usage in a connection
   pool, and also because an actual websocket connection can suffer redirects
   before it succeeds, etc. *)

val send : connection -> request -> response promise

val no_pool :
  ?transport:[ `HTTP1 | `HTTPS | `HTTP2 ] -> request -> response promise
(* Used for testing. *)
