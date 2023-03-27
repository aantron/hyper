(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



module Httpaf = Dream_httpaf_.Httpaf
module Httpaf_lwt_unix = Dream_httpaf__lwt_unix.Httpaf_lwt_unix

module Message = Dream_pure.Message
module Method = Dream_pure.Method
module Stream = Dream_pure.Stream



(** Runs a request for both cleartext and SSL HTTP/1.1 connections, since the
    code is the same once the request is created. *)
let general send_request client connection (request : Message.request) =

  (* The http/af request can be created right away. *)
  let httpaf_request : Httpaf.Request.t =
    Message.set_content_length_headers request;
    let headers =
      Message.all_headers request
      |> Httpaf.Headers.of_list
    and method_ =
      Httpaf.Method.of_string
        (Method.method_to_string (Message.method_ request))
    and target = Uri.path_and_query (Uri.of_string (Message.target request)) in

    Httpaf.Request.create ~headers method_ target
  in

  (* The http/af response and Hyper response are delayed, so create a
     promise. *)
  let response_promise, receive_response = Lwt.wait () in

  (* Exceptions passed to the error handler after the response handler is called
     (i.e. when the response body is being read) are stored here, and are raised
     by the response body reader. *)
  let received_response = ref false in
  let reported_exn = ref None in
  let exn_handler = ref ignore in

  (* TODO Propagate the close and abort changes to the server side, HTTP/2,
     etc. *)
  let response_handler
      (httpaf_response : Httpaf.Response.t) httpaf_response_body =

    received_response := true;

    (* Doing a second schedule_read after on_eof results in no callback ever
       being called. We want to call ~close or ~exn again, though. Using
       is_closed instead of this reference is not good enough, because is_closed
       becomes true while there is still data in the stream, and schedule_read
       would succeed. *)
    let got_eof = ref false in

    let read ~data ~flush:_ ~ping:_ ~pong:_ ~close ~exn =
      match !reported_exn with
      | Some the_exn ->
        exn the_exn
      | None ->
        if !got_eof then
          close 1000
        else begin
          exn_handler := exn;
          Httpaf.Body.Reader.schedule_read
            httpaf_response_body
            ~on_eof:(fun () ->
              got_eof := true;
              exn_handler := ignore;
              close 1000)
            ~on_read:(fun buffer ~off ~len ->
              exn_handler := ignore;
              data buffer off len true false)
        end

    and close _code =
      Httpaf.Body.Reader.close httpaf_response_body

    and abort exn =
      reported_exn := Some exn;
      Httpaf.Client_connection.report_exn connection exn

    in

    let client_stream =
      Stream.stream (Stream.reader ~read ~close ~abort) Stream.no_writer in

    Message.response
      ~code:(Httpaf.Status.to_code httpaf_response.status)
      ~headers:(Httpaf.Headers.to_list httpaf_response.headers)
      client_stream
      Stream.null
    |> Lwt.wakeup_later receive_response
  in

  let error_handler = function
    | `Malformed_response explanation ->
      Lwt.wakeup_later_exn receive_response
        (Failure ("malformed response: " ^ explanation))
    | `Invalid_response_body_length _response ->
      Lwt.wakeup_later_exn receive_response
        (Failure "invalid response body length")
    | `Exn exn ->
      if not !received_response then
        Lwt.wakeup_later_exn receive_response exn
      else begin
        reported_exn := Some exn;
        let handler = !exn_handler in
        exn_handler := ignore;
        handler exn
      end
  in

  let httpaf_request_body_writer =
    send_request
      client
      httpaf_request
      ~error_handler
      ~response_handler
  in

  (* The request body writing loop. *)
  let bytes_since_flush = ref 0 in

  let rec send () =
    Stream.read
      (Message.server_stream request) ~data ~flush ~ping ~pong ~close ~exn

  and data buffer offset length _binary _fin =
    Httpaf.Body.Writer.write_bigstring
      httpaf_request_body_writer
      ~off:offset
      ~len:length
      buffer;
    bytes_since_flush := !bytes_since_flush + length;
    if !bytes_since_flush >= 4096 then begin
      bytes_since_flush := 0;
      Httpaf.Body.Writer.flush httpaf_request_body_writer send
    end
    else
      send ()

  and flush () =
    bytes_since_flush := 0;
    Httpaf.Body.Writer.flush httpaf_request_body_writer send

  and ping _buffer _offset _length =
    send ()

  and pong _buffer _offset _length =
    send ()

  and close _code =
    Httpaf.Body.Writer.close httpaf_request_body_writer

  and exn exn =
    Httpaf.Client_connection.report_exn connection exn in

  send ();

  response_promise



let http client =
  general Httpaf_lwt_unix.Client.request client client.connection

let https client =
  general Httpaf_lwt_unix.Client.SSL.request client client.connection
