(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



module Formats = Dream_pure.Formats
module Logic = Hyper__logic
module Message = Dream_pure.Message
module Method = Dream_pure.Method
module Status = Dream_pure.Status
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



let request ?method_ ?headers ?(body = "") target =
  let request =
    Message.request ?method_ ~target ?headers Stream.null Stream.null in
  Message.set_body request body;
  request

let stream ?method_ ?headers ?(close = true) target callback =
  let reader, writer = Stream.pipe () in
  let client_stream = Stream.stream Stream.no_reader writer
  and server_stream = Stream.stream reader Stream.no_writer in
  let request =
    Message.request ?method_ ~target ?headers client_stream server_stream in

  Lwt.async begin fun () ->
    if close then
      match%lwt callback client_stream with
      | () ->
        Message.close client_stream
      | exception exn ->
        let%lwt () = Message.close client_stream in
        raise exn
    else
      callback client_stream
  end;

  request

(* TODO Rename this. *)
let send = Hyper__http.Connect.no_pool ?transport:None

let body = Message.body

(* let follow_redirect = Hyper__logic.Redirect.follow_redirect *)

let to_form_urlencoded = Formats.to_form_urlencoded



(* let read message =
  Message.read (Message.client_stream message)

let write ?kind message chunk =
  Message.write ?kind (Message.client_stream message) chunk

let flush message =
  Message.flush (Message.client_stream message) *)



let default_middlewares redirect_limit =
  Message.pipeline [
    Logic.Headers.set_user_agent_header;
    Logic.Redirect.follow_redirect ?redirect_limit;
    Logic.Headers.set_host_header;
  ]

let run ?redirect_limit ?(server = send) request =
  default_middlewares redirect_limit server request

exception Response of response

let () =
  Printexc.register_printer begin function
    | Response response ->
      let status = Message.status response in
      let reason =
        match Status.status_to_reason status with
        | None -> ""
        | Some reason -> " " ^ reason
      in
      Printf.sprintf "Hyper.Response(%i%s)" (Status.status_to_int status) reason
      |> Option.some
    | _ ->
      None
  end

(* TODO Note in the docs that the response body is closed. *)
(* TODO LATER Consider eagerly reading 1K or 4K of the body before closing. *)
let raise_response response =
  let%lwt () = Message.close (Message.client_stream response) in
  raise (Response response)

(* TODO Remove most optional args from these high-level functions. *)
let get ?headers ?redirect_limit ?(server = send) target =
  let request =
    request
      ~method_:`GET
      ?headers
      target
  in
  let%lwt response = run ?redirect_limit ~server request in
  if Message.status response = `OK then
    body response
  else
    raise_response response

let post ?(headers = []) ?redirect_limit ?(server = send) target the_body =
  let request =
    request
      ~method_:`POST
      ~headers
      ~body:the_body
      target
  in
  let%lwt response = run ?redirect_limit ~server request in
  if Message.status response = `OK then
    body response
  else
    raise_response response

(* TODO Move this to message.ml. *)
type websocket =
  Stream.stream * Stream.stream

let websocket ?redirect_limit ?(server = send) target =
  let request = request ~method_:`GET target in
  let%lwt response = run ?redirect_limit ~server request in
  match Message.get_websocket response with
  | Some websocket -> Lwt.return websocket
  | None -> raise_response response

let send ?text_or_binary ?end_of_message (client_stream, _) data =
  Message.send ?text_or_binary ?end_of_message client_stream data

let receive (client_stream, _) =
  Message.receive client_stream

let receive_fragment (client_stream, _) =
  Message.receive_fragment client_stream

let close_websocket = Message.close_websocket
