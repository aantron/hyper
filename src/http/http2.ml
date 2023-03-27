(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



module H2 = Dream_h2.H2
module H2_lwt_unix = Dream_h2_lwt_unix.H2_lwt_unix

module Message = Dream_pure.Message
module Method = Dream_pure.Method
module Stream = Dream_pure.Stream



(* This code is very similar to http1.ml. *)
let https (connection : H2_lwt_unix.Client.SSL.t) (request : Message.request) =

  let h2_request : H2.Request.t =
    Message.drop_content_length_headers request;
    Message.lowercase_headers request;
    let headers =
      Message.all_headers request
      |> H2.Headers.of_list
    and scheme = "https"
    and method_ =
      H2.Method.of_string (Method.method_to_string (Message.method_ request))
    and target = Uri.path_and_query (Uri.of_string (Message.target request)) in

    H2.Request.create ~headers ~scheme method_ target
  in

  let response_promise, receive_response = Lwt.wait () in

  (* TODO NOTE The HTTP/2 error handling implemented in h2 appears to be about
     the same as HTTP/1 error handling in http/af. It's not clear that it is
     possible to cancel individual streams in response to exceptions. *)
  let received_response = ref false in
  let reported_exn = ref None in
  let exn_handler = ref ignore in

  let report_error exn =
    if not !received_response then
      Lwt.wakeup_later_exn receive_response exn
    else begin
      reported_exn := Some exn;
      let handler = !exn_handler in
      exn_handler := ignore;
      handler exn
    end
  in

  let error_handler = function
    | `Malformed_response explanation ->
      report_error (Failure ("malformed response: " ^ explanation))
    | `Invalid_response_body_length _response ->
      report_error (Failure "invalid response body length")
    | `Protocol_error (code, _explanation) ->
      report_error
        (Failure ("protocol error: " ^ (H2.Error_code.to_string code)))
    | `Exn exn ->
      report_error exn
  in

  let response_handler (h2_response : H2.Response.t) h2_response_body =
    received_response := true;

    let read ~data ~flush:_ ~ping:_ ~pong:_ ~close ~exn =
      match !reported_exn with
      | Some the_exn ->
        exn the_exn
      | None ->
        exn_handler := exn;
        H2.Body.schedule_read
          h2_response_body
          ~on_eof:(fun () ->
            exn_handler := ignore;
            close 1000)
          ~on_read:(fun buffer ~off ~len ->
            exn_handler := ignore;
            data buffer off len true false)

    and close _code =
      H2.Body.close_reader h2_response_body

    and abort exn =
      H2.Client_connection.report_exn connection.connection exn;
      report_error exn in

    let client_stream =
      Stream.stream (Stream.reader ~read ~close ~abort) Stream.no_writer in

    Message.response
      ~code:(H2.Status.to_code h2_response.status)
      ~headers:(H2.Headers.to_list h2_response.headers)
      client_stream
      Stream.null
    |> Lwt.wakeup_later receive_response
  in

  let h2_request_body_writer =
    H2_lwt_unix.Client.SSL.request
      connection
      h2_request
      ~error_handler
      ~response_handler
  in

  let bytes_since_flush = ref 0 in

  let rec send () =
    Stream.read
      (Message.server_stream request) ~data ~flush ~ping ~pong ~close ~exn

  and data buffer offset length _binary _fin =
    H2.Body.write_bigstring
      h2_request_body_writer
      ~off:offset
      ~len:length
      buffer;
    bytes_since_flush := !bytes_since_flush + length;
    if !bytes_since_flush >= 4096 then begin
      bytes_since_flush := 0;
      H2.Body.flush h2_request_body_writer send
    end
    else
      send ()

  and flush () =
    bytes_since_flush := 0;
    H2.Body.flush h2_request_body_writer send

  and ping _buffer _offset _length =
    send ()

  and pong _buffer _offset _length =
    send ()

  and close _code =
    H2.Body.close_writer h2_request_body_writer

  and exn exn =
    H2.Client_connection.report_exn connection.connection exn;
    report_error exn in

  send ();

  response_promise
