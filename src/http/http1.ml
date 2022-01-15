(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2021 Anton Bachin *)



module Message = Dream_pure.Message
module Method = Dream_pure.Method
module Stream = Dream_pure.Stream



(** Runs a request for both cleartext and SSL HTTP/1.1 connections, since the
    code is the same once the request is created. *)
(* TODO How to best pass the done functions. *)
let general make_request connection hyper_request =
  let response_promise, received_response = Lwt.wait () in

  let response_handler
      (httpaf_response : Httpaf.Response.t)
      httpaf_response_body =

    (* TODO A bit annoying that this has to be bound first, because the reader
       needs a reference to the response in order to report the body reading as
       finished to the connection pool. *)
    (* TODO response creation can now be pushed down, since the _done mechanism
       is gone. *)
    let hyper_response =
      Message.response
        ~code:(Httpaf.Status.to_code httpaf_response.status)
        ~headers:(Httpaf.Headers.to_list httpaf_response.headers)
        Stream.null
        Stream.null
    in

    (* TODO This code is very similar to the server side, for requests. *)
    (* TODO Work out closing. In particular, in case of early close by the
       reader, this should probably be treated as an error for the purposes of
       connection pipelining, so the connection should not be reused because the
       state of the response stream relative to expectations is unknown. That
       can actually destroy request pipelining, since the next request may have
       already started writing. Is this one of the reasons pipelining is
       broken in most implementations? *)
    let read ~data ~flush:_ ~ping:_ ~pong:_ ~close ~exn:_ =
      Httpaf.Body.Reader.schedule_read
        httpaf_response_body
        ~on_eof:(fun () ->
          close 1000)
        ~on_read:(fun buffer ~off ~len ->
          data buffer off len true false)
    in
    let close _code =
      Httpaf.Body.Reader.close httpaf_response_body in
    let client_stream =
      Stream.stream
        (Stream.reader ~read ~close ~abort:close) Stream.no_writer in
    Message.set_client_stream hyper_response client_stream;

    Lwt.wakeup_later received_response hyper_response
  in

  let httpaf_request =
    Httpaf.Request.create
      ~headers:(Httpaf.Headers.of_list (Message.all_headers hyper_request))
      (Httpaf.Method.of_string
        (Method.method_to_string (Message.method_ hyper_request)))
      (Uri.path_and_query (Uri.of_string (Message.target hyper_request))) in
  let httpaf_request_body =
    make_request
      connection
      httpaf_request
      ~error_handler:(fun _ -> failwith "Protocol error") (* TODO *)
      ~response_handler in

  let rec send () =
    Stream.read
      (Message.server_stream hyper_request) ~data ~flush ~ping ~pong ~close ~exn

  (* TODO Implement flow control like on the server side, using flush. *)
  and data buffer offset length _binary _fin =
    Httpaf.Body.Writer.write_bigstring
      httpaf_request_body
      ~off:offset
      ~len:length
      buffer;
    send ()

  and flush () = send ()
  and ping _buffer _offset _length = send ()
  and pong _buffer _offset _length = send ()

  and close _code =
    Httpaf.Body.Writer.close httpaf_request_body
  and exn _exn =
    Httpaf.Body.Writer.close httpaf_request_body

  in

  send ();

  response_promise



let http =
  general Httpaf_lwt_unix.Client.request

let https =
  general Httpaf_lwt_unix.Client.SSL.request
