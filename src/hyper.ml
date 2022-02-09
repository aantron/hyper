(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



module Formats = Dream_pure.Formats
module Message = Dream_pure.Message
module Method = Dream_pure.Method
module Stream = Dream_pure.Stream



type request = client message
and response = server message
and handler = request -> response promise
(* and middleware = hansdler -> handler *)
and 'a message = 'a Message.message
and client = Message.client
and server = Message.server
and 'a promise = 'a Lwt.t
and stream = Stream.stream

type method_ = Method.method_



(* TODO There is no way to create a request with a pipe using this function. *)
let request ?method_ ?headers ?(body = Stream.empty) target =
  Message.request ?method_ ~target ?headers Stream.null body

let send = Hyper__http.Connect.no_pool ?transport:None

let body = Message.body

let follow_redirect = Hyper__logic.Redirect.follow_redirect

let to_form_urlencoded = Formats.to_form_urlencoded



let read message =
  Message.read (Message.client_stream message)

let write ?kind message chunk =
  Message.write ?kind (Message.client_stream message) chunk

let flush message =
  Message.flush (Message.client_stream message)



(* TODO Error handling. *)
let get ?headers ?redirect_limit ?(server = send) target =
  let request =
    request
      ~method_:`GET
      ?headers
      target
  in
  let%lwt response = (follow_redirect ?redirect_limit server) request in
  body response

(* TODO Error handling. *)
(* TODO Don't always use TE: chunked. Also, this should be handled at the
   transport level, since TE: chunked is not even valid for http2. *)
let post ?(headers = []) ?redirect_limit ?(server = send) target the_body =
  let headers = ("Transfer-Encoding", "chunked")::headers in
  let request =
    request
      ~method_:`POST
      ~headers
      ~body:(Stream.string the_body)
      target
  in
  let%lwt response = (follow_redirect ?redirect_limit server) request in
  body response

let websocket ?redirect_limit ?(server = send) target =
  let request = request ~method_:`GET target in
  (follow_redirect ?redirect_limit server) request
  (* TODO Check the response status... *)
