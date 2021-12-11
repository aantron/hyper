(* This file is part of Dream, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/dream.

   Copyright 2021 Anton Bachin *)



let https ~all_done connection hyper_request =
  let response_promise, received_response = Lwt.wait () in

  (* TODO This is a nasty duplicate of the above case, specialized for H2. See
     comments above. *)
  let response_handler (h2_response : H2.Response.t) h2_response_body =

    Lwt.async begin fun () ->
      let%lwt hyper_response =
        Dream.stream
          ~code:(H2.Status.to_code h2_response.status)
          ~headers:(H2.Headers.to_list h2_response.headers)
          (fun _response -> Lwt.return ())
      in
      Lwt.wakeup_later received_response hyper_response;

      let rec receive () =
        H2.Body.schedule_read
          h2_response_body
          ~on_eof:(fun () ->
            Lwt.async (fun () ->
              let%lwt () = Dream.close_stream hyper_response in
              all_done hyper_response;
              Lwt.return_unit))
          ~on_read:(fun buffer ~off ~len ->
            Lwt.async (fun () ->
              let%lwt () =
                Dream.write_buffer
                  ~offset:off ~length:len hyper_response buffer in
              Lwt.return (receive ())))
      in
      receive ();

      Lwt.return ()
    end
  in

  let h2_request =
    H2.Request.create
      ~headers:(H2.Headers.of_list (Dream.all_headers hyper_request))
      ~scheme:"https"
      (Httpaf.Method.of_string
        (Dream.method_to_string (Dream.method_ hyper_request)))
      (Uri.path_and_query (Uri.of_string (Dream.target hyper_request))) in
  let h2_request_body =
    H2_lwt_unix.Client.SSL.request
      connection
      h2_request
      ~error_handler:(fun _ -> failwith "Protocol error")
      ~response_handler in

  let rec send () =
    Dream.server_stream hyper_request
    |> fun stream ->
      Dream.next stream ~data ~close ~flush ~ping ~pong

  and data buffer offset length _binary _fin =
    H2.Body.write_bigstring
      h2_request_body
      ~off:offset
      ~len:length
      buffer;
    send ()

  and close _code = H2.Body.close_writer h2_request_body
  and flush () = send ()
  and ping _buffer _offset _length = send ()
  and pong _buffer _offset _length = send ()

  in

  send ();

  response_promise
