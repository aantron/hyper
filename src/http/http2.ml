(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2021 Anton Bachin *)



module Message = Dream_pure.Message
module Method = Dream_pure.Method
module Stream = Dream_pure.Stream



let https connection hyper_request =
  let response_promise, received_response = Lwt.wait () in

  (* TODO This is a nasty duplicate of the above case, specialized for H2. See
     comments above. *)
  let response_handler (h2_response : H2.Response.t) h2_response_body =

    let hyper_response =
      Message.response
        ~code:(H2.Status.to_code h2_response.status)
        ~headers:(H2.Headers.to_list h2_response.headers)
        Stream.null
        Stream.null
    in

    let read ~data ~flush:_ ~ping:_ ~pong:_ ~close ~exn:_ =
      H2.Body.schedule_read
        h2_response_body
        ~on_eof:(fun () ->
          close 1000)
        ~on_read:(fun buffer ~off ~len ->
          data buffer off len true false)
    in
    let close _code =
      H2.Body.close_reader h2_response_body in
    let client_stream =
      Stream.stream
      (Stream.reader ~read ~close ~abort:close) Stream.no_writer in
    Message.set_client_stream hyper_response client_stream;

    Lwt.wakeup_later received_response hyper_response
  in

  let h2_request =
    H2.Request.create
      ~headers:(H2.Headers.of_list (Message.all_headers hyper_request))
      ~scheme:"https"
      (Httpaf.Method.of_string
        (Method.method_to_string (Message.method_ hyper_request)))
      (Uri.path_and_query (Uri.of_string (Message.target hyper_request))) in
  let h2_request_body =
    H2_lwt_unix.Client.SSL.request
      connection
      h2_request
      ~error_handler:(fun _ -> failwith "Protocol error")
      ~response_handler in

  let rec send () =
    Stream.read
      (Message.server_stream hyper_request) ~data ~flush ~ping ~pong ~close ~exn

  and data buffer offset length _binary _fin =
    H2.Body.write_bigstring
      h2_request_body
      ~off:offset
      ~len:length
      buffer;
    send ()

  and flush () = send ()
  and ping _buffer _offset _length = send ()
  and pong _buffer _offset _length = send ()
  and close _code = H2.Body.close_writer h2_request_body
  and exn _exn = H2.Body.close_writer h2_request_body

  in

  send ();

  response_promise
