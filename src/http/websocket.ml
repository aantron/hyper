(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



module Message = Dream_pure.Message
module Stream = Dream_pure.Stream



let () =
  Mirage_crypto_rng_lwt.initialize ()



let ws socket request =
  let target = Uri.of_string (Message.target request) in

  (* Retrieve values for the Host: and Port: headers of the WebSocket
     request. *)
  let host = Option.value (Uri.host target) ~default:"" in
  let port =
    match Uri.port target with
    | Some port -> port
    | None ->
      match Uri.scheme target with
      | Some "wss" -> 443
      | _ -> 80
  in

  let response_promise, receive_response = Lwt.wait () in

  let received_response = ref false in
  let reported_exn = ref None in
  let exn_handler = ref ignore in

  let error_handler = function
    | `Handshake_failure (httpaf_response, httpaf_response_body_reader) ->

      received_response := true;
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
              httpaf_response_body_reader
              ~on_eof:(fun () ->
                got_eof := true;
                exn_handler := ignore;
                close 1000)
              ~on_read:(fun buffer ~off ~len ->
                exn_handler := ignore;
                data buffer off len true false)
          end

      and close _code =
        Httpaf.Body.Reader.close httpaf_response_body_reader

      and abort _exn =
        (* TODO Not clear how to report the exception; see
           https://github.com/anmonteiro/websocketaf/issues/40. *)
        Httpaf.Body.Reader.close httpaf_response_body_reader in

      let client_stream =
        Stream.stream (Stream.reader ~read ~close ~abort) Stream.no_writer in

      Message.response
        ~code:(Httpaf.Status.to_code httpaf_response.Httpaf.Response.status)
        ~headers:(Httpaf.Headers.to_list httpaf_response.headers)
        client_stream
        Stream.null
      |> Lwt.wakeup_later receive_response

    | `Malformed_response explanation ->
      Lwt.wakeup_later_exn receive_response
        (Failure ("malformed response: " ^ explanation))
    | `Invalid_response_body_length _ ->
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

  let websocket_handler socket =
    let response =
      Message.response ~status:`Switching_Protocols Stream.empty Stream.null in
    let server_stream = Message.create_websocket response in
    Lwt.wakeup_later receive_response response;
    Dream_httpaf.Websocket.client_websocket_handler server_stream socket
  in

  let%lwt client =
    Websocketaf_lwt_unix.Client.connect
      ~nonce:(Cstruct.to_string (Mirage_crypto_rng.generate 16))
      ~host
      ~port
      ~resource:(Uri.path_and_query target)
      ~error_handler
      ~websocket_handler
      socket
  in
  ignore client;

  response_promise
