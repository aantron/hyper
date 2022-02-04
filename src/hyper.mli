(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



type request = client message
and response = server message
and handler = request -> response promise
(* and middleware = handler -> handler *)
and 'a message = 'a Dream_pure.Message.message
and client = Dream_pure.Message.client
and server = Dream_pure.Message.server
and 'a promise = 'a Lwt.t
and stream = Dream_pure.Stream.stream



val get :
  ?headers:(string * string) list ->
  ?redirect_limit:int ->
  ?server:handler ->
    string -> string promise

val post :
  ?headers:(string * string) list ->
  ?redirect_limit:int ->
  ?server:handler ->
    string -> string -> string promise

val websocket :
  (* ?headers:(string * string) list -> *)
  ?redirect_limit:int ->
  ?server:handler ->
    string -> response promise
    (* TODO This shouldn't return a low-level stream. *)



(* TODO Import the whole method set, etc. *)
type method_ = Dream_pure.Method.method_

(* TODO How should the body be exposed? *)
val request :
  ?method_:[< method_ ] ->
  ?headers:(string * string) list ->
  ?body:stream ->
  string ->
    request

val send : request -> response promise

val body : 'a message -> string promise

val to_form_urlencoded : (string * string) list -> string



val read : 'a message -> string option promise

val write : ?kind:[< `Text | `Binary ] -> 'a message -> string -> unit promise

val flush : 'a message -> unit promise



(* type connection *)

(* TODO But factor out DNS somehow. Probably as an optional to
   connection_pool. *)
(* val http1_cleartext_tcp : string -> connection promise
val alpn_https_tcp : string -> connection promise
val send_over : connection -> request -> response promise
val send_one : request -> response promise *)
(* val connection_pool : request -> response promise *)

(* val redirections : ?redirect_limit:int -> middleware *)
(* val relative_to : string -> middleware *)
