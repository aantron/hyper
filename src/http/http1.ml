(* This file is part of Dream, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/dream.

   Copyright 2021 Anton Bachin *)



(** Runs a request for both cleartext and SSL HTTP/1.1 connections, since the
    code is the same once the request is created. *)
(* TODO How to best pass the done functions. *)
let general make_request ~write_done ~all_done connection hyper_request =
  let response_promise, received_response = Lwt.wait () in

  (* TODO Do we now want to store the verson? *)
  let response_handler
      (httpaf_response : Httpaf.Response.t)
      httpaf_response_body =

    (* TODO Using Dream.stream is awkward here, but it allows getting a response
       with a stream inside it without immeidately having to modify Dream. Once
       that is fixed, the Lwt.async can be removed, most likely. Dream.stream's
       signature will change in Dream either way, so it's best to just hold off
       tweaking it now. *)
    Lwt.async begin fun () ->
      let%lwt hyper_response =
        Dream.stream
          ~code:(Httpaf.Status.to_code httpaf_response.status)
          ~headers:(Httpaf.Headers.to_list httpaf_response.headers)
          (fun _response -> Lwt.return ())
      in
      Lwt.wakeup_later received_response hyper_response;

      (* TODO A janky reader. Once Dream.stream is fixed and streams are fully
         exposed, this can become a good pull-reader. *)
      let rec receive () =
        Httpaf.Body.Reader.schedule_read
          httpaf_response_body
          ~on_eof:(fun () ->
            Lwt.async (fun () ->
              let%lwt () = Dream.close_stream hyper_response in
              all_done hyper_response;
              Lwt.return_unit))
              (* TODO Make sure there is a way for the reader to abort reading
                 the stream and yet still get the socket closed. *)
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

  let httpaf_request =
    Httpaf.Request.create
      ~headers:(Httpaf.Headers.of_list (Dream.all_headers hyper_request))
      (Httpaf.Method.of_string
        (Dream.method_to_string (Dream.method_ hyper_request)))
      (Uri.path_and_query (Uri.of_string (Dream.target hyper_request))) in
  let httpaf_request_body =
    make_request
      connection
      httpaf_request
      ~error_handler:(fun _ -> failwith "Protocol error") (* TODO *)
      ~response_handler in

  let rec send () =
    Dream.server_stream hyper_request
    |> fun stream ->
      Dream.next stream ~data ~close ~flush ~ping ~pong

  (* TODO Implement flow control like on the server side, using flush. *)
  and data buffer offset length _binary _fin =
    Httpaf.Body.Writer.write_bigstring
      httpaf_request_body
      ~off:offset
      ~len:length
      buffer;
    send ()

  and close _code =
    Httpaf.Body.Writer.close httpaf_request_body;
    (* TODO This should only be called if reading is not yet done. *)
    write_done ()

  and flush () = send ()
  and ping _buffer _offset _length = send ()
  and pong _buffer _offset _length = send ()

  in

  send ();

  response_promise



let http =
  general Httpaf_lwt_unix.Client.request

let https =
  general Httpaf_lwt_unix.Client.SSL.request
