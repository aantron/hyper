(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2021 Anton Bachin *)



type connection
type 'a promise = 'a Dream_pure.Message.promise
type request = Dream_pure.Message.request
type response = Dream_pure.Message.response

val close : connection -> unit promise

val http1_cleartext_tcp : string -> connection promise
val alpn_https_tcp : string -> connection promise
(* TODO This signature is basically useless in the long term. It should give
   some kind of scheme, host, port tuple instead of an opaque function for
   establishing a connection. *)

val send : connection -> request -> response promise
val no_pool : request -> response promise
