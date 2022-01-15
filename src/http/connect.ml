(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2021 Anton Bachin *)



module Message = Dream_pure.Message

type response = Message.response
type request = Message.request



(* TODO Is this the right representation? *)
type connection =
  | Cleartext of Httpaf_lwt_unix.Client.t (* TODO Rename constructor. *)
  | SSL of Httpaf_lwt_unix.Client.SSL.t
  | H2 of H2_lwt_unix.Client.SSL.t (* TODO No h2c support. *)
  (* | WebSocket of Stream.stream *)
    (* TODO NOTE WebSocket connections over HTTP/1.1 are currently
       single-use. We still go through the pool so as to give it the chance to
       refuse the connection based on the number of other connections to the
       same endpoint or host. The actual closing of WebSocket connections by the
       pool is not yet implemented, so it might try to multiplex them. *)
    (* TODO WebSockets over https and WebSockets over HTTP/2. *)

type 'a promise = 'a Dream_pure.Message.promise

let close = function
  | Cleartext connection -> Httpaf_lwt_unix.Client.shutdown connection
  | SSL connection -> Httpaf_lwt_unix.Client.SSL.shutdown connection
  | H2 connection -> H2_lwt_unix.Client.SSL.shutdown connection

(* let concurrency = function
  | Cleartext _ -> `Pipeline
  | SSL _ -> `Pipeline
  | H2 _ -> `Multiplex *)

(* TODO How to really do DNS? Especially with IPv6 and QUIC (UDP). *)
let resolve target =
  let uri = Uri.of_string target in
  let host = Uri.host uri |> Option.get (* TODO Questonable. *)
  and port = Uri.port uri |> Option.value ~default:80
  in
  let%lwt addresses =
    Lwt_unix.getaddrinfo host (string_of_int port) [Unix.(AI_FAMILY PF_INET)] in
  let address = (List.hd addresses).Unix.ai_addr in
  Lwt.return address

(* TODO Network error handling. *)
let http1_cleartext_tcp target =
  let socket = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  let%lwt address = resolve target in
  let%lwt () = Lwt_unix.connect socket address in
  let%lwt connection = Httpaf_lwt_unix.Client.create_connection socket in
  Lwt.return (Cleartext connection)

let alpn_https_tcp target =
  (* TODO The context needs to be created once per process, or a cache
     should be used. *)
  let context = Ssl.(create_context TLSv1_2 Client_context) in
  (* TODO For WebSockets (wss://), the client should probably do SSL
      without offering h2 by ALPN. Do any servers implement WebSockets over
      HTTP/2? *)
  Ssl.set_context_alpn_protos context ["h2"; "http/1.1"];

  let socket = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  let%lwt address = resolve target in
  let%lwt () = Lwt_unix.connect socket address in
  let%lwt ssl_socket = Lwt_ssl.ssl_connect socket context in
  (* TODO Next line is pretty suspicious. *)
  let underlying = Lwt_ssl.ssl_socket ssl_socket |> Option.get in
  begin match Ssl.get_negotiated_alpn_protocol underlying with
  | Some "h2" ->
    (* TODO What about the error handler? *)
    let%lwt connection =
      H2_lwt_unix.Client.SSL.create_connection
        ~error_handler:ignore
        ssl_socket
    in
    Lwt.return (H2 connection)
  | _ -> (* TODO Match http/1.1 or None, while Some _ should be an error. *)
    let%lwt connection =
      Httpaf_lwt_unix.Client.SSL.create_connection ssl_socket in
    Lwt.return (SSL connection)
  end
  (* TODO Need to do server certificate validation here, etc. *)

let choose request =
  let scheme =
    Message.target request
    |> Uri.of_string
    |> Uri.scheme
    |> Option.get (* TODO Questionable. *)
  in
  if scheme = "https" then
    alpn_https_tcp
  else
    http1_cleartext_tcp

let send connection request =
  match connection with
  | Cleartext connection -> Http1.http connection request
  | SSL connection -> Http1.https connection request
  | H2 connection -> Http2.https connection request

let no_pool request =
  let connect = choose request in
  let%lwt connection = connect (Message.target request) in
  send connection request
  (* TODO Have to monitor the closing of the response stream, and close the
     connection when that occurs. *)
