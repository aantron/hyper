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
let request ?method_ ?headers ?body target =
  match body with
  | None ->
    let request =
      Message.request ?method_ ~target ?headers Stream.null Stream.null in
    Message.set_body request "";
    request
  | Some stream ->
    Message.request ?method_ ~target ?headers Stream.null stream

(* TODO Rename this. *)
let send = Hyper__http.Connect.no_pool ?transport:None
let run = send

let body = Message.body

let follow_redirect = Hyper__logic.Redirect.follow_redirect

let to_form_urlencoded = Formats.to_form_urlencoded



(* let read message =
  Message.read (Message.client_stream message)

let write ?kind message chunk =
  Message.write ?kind (Message.client_stream message) chunk

let flush message =
  Message.flush (Message.client_stream message) *)



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
let post ?(headers = []) ?redirect_limit ?(server = send) target the_body =
  let request =
    request
      ~method_:`POST
      ~headers
      ~body:(Stream.string the_body)
      target
  in
  let%lwt response = (follow_redirect ?redirect_limit server) request in
  body response

(* TODO Move this to message.ml. *)
type websocket =
  Stream.stream * Stream.stream

let websocket ?redirect_limit ?(server = send) target =
  let request = request ~method_:`GET target in
  let%lwt response = (follow_redirect ?redirect_limit server) request in
  match Message.get_websocket response with
  | None -> assert false (* TODO Real error. *)
  | Some websocket -> Lwt.return websocket

let send ?text_or_binary ?end_of_message (client_stream, _) data =
  Message.send ?text_or_binary ?end_of_message client_stream data

let receive (client_stream, _) =
  Message.receive client_stream

let receive_fragment (client_stream, _) =
  Message.receive_fragment client_stream

let close_websocket = Message.close_websocket
