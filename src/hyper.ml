(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



module Formats = Dream_pure.Formats
module Logic = Hyper__logic
module Message = Dream_pure.Message
module Method = Dream_pure.Method
module Status = Dream_pure.Status
module Stream = Dream_pure.Stream



(* Types *)

type request = client message
and response = server message
and handler = request -> response promise
and middleware = handler -> handler

and 'a message = 'a Message.message
and client = Message.client
and server = Message.server
and 'a promise = 'a Lwt.t



(* Methods *)

include Method



(* Status codes *)

include Status



(* Requests *)

let request ?method_ ?headers ?(body = "") target =
  let request =
    Message.request ?method_ ~target ?headers Stream.null Stream.null in
  Message.set_body request body;
  request

let default_middlewares redirect_limit =
  Message.pipeline [
    Logic.Headers.set_user_agent_header;
    Logic.Redirect.follow_redirect ?redirect_limit;
    Logic.Headers.set_host_header;
  ]

let connect = Hyper__http.Connect.no_pool ?transport:None

let run ?redirect_limit ?(server = connect) request =
  default_middlewares redirect_limit server request



(* Responses *)

let status = Message.status
let body = Message.body



(* Headers *)

let header = Message.header
let headers = Message.headers
let all_headers = Message.all_headers
let has_header = Message.has_header
let add_header = Message.add_header
let drop_header = Message.drop_header
let set_header = Message.set_header



(* Streams *)

type stream = Stream.stream

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

let body_stream = Message.client_stream
let read = Message.read
let write = Message.write
let flush = Message.flush
let close = Message.close



(* WebSockets *)

type websocket =
  Stream.stream * Stream.stream

let websocket ?(headers = []) ?redirect_limit ?(server = connect) target =
  let request = request ~method_:`GET ~headers target in
  let%lwt response = run ?redirect_limit ~server request in
  match Message.get_websocket response with
  | Some websocket -> Lwt.return (Ok websocket)
  | None -> Lwt.return (Error response)

type text_or_binary = [ `Text | `Binary ]
type end_of_message = [ `End_of_message | `Continues ]

let send ?text_or_binary ?end_of_message (client_stream, _) data =
  Message.send ?text_or_binary ?end_of_message client_stream data

let receive (client_stream, _) =
  Message.receive client_stream

let receive_fragment (client_stream, _) =
  Message.receive_fragment client_stream

let close_websocket = Message.close_websocket



(* Web formats *)

let to_form_urlencoded = Formats.to_form_urlencoded



(* Quick use *)

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

let get ?headers target =
  let request =
    request
      ~method_:`GET
      ?headers
      target
  in
  Lwt_main.run begin
    let%lwt response = run request in
    if Message.status response = `OK then
      body response
    else
      raise_response response
  end

let post ?(headers = []) target the_body =
  let request =
    request
      ~method_:`POST
      ~headers
      ~body:the_body
      target
  in
  Lwt_main.run begin
    let%lwt response = run request in
    if Message.status response = `OK then
      body response
    else
      raise_response response
  end
