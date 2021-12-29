(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2021 Anton Bachin *)



module Message = Dream_pure.Message
module Stream = Dream_pure.Stream



type request = Message.request
type response = Message.response
type 'a promise = 'a Lwt.t

type method_ = Dream_pure.Method.method_



let request ?method_ ?headers target =
  Message.request ?method_ ~target ?headers Stream.null Stream.empty

let body response =
  Message.body response



(* TODO Is this the right representation? *)
type connection =
  | Cleartext of Httpaf_lwt_unix.Client.t
  | SSL of Httpaf_lwt_unix.Client.SSL.t
  | H2 of H2_lwt_unix.Client.SSL.t (* TODO No h2c support. *)
  (* | WebSocket of Stream.stream *)
    (* TODO NOTE WebSocket connections over HTTP/1.1 are currently
       single-use. We still go through the pool so as to give it the chance to
       refuse the connection based on the number of other connections to the
       same endpoint or host. The actual closing of WebSocket connections by the
       pool is not yet implemented, so it might try to multiplex them. *)
    (* TODO WebSockets over https and WebSockets over HTTP/2. *)

type endpoint = string * string * int
(* TODO But what should a host be? An unresolved hostname:port, or a resolved
   hostname:port? Hosts probably also need a comparison function or
   something. And probably a pretty-printing function. These things are entirely
   abstract. But, because they are abstract, it's possible to change the
   implementation, in particular to switch from unresolved to resolved hosts.
   Using unresolved hosts implies doing DNS before deciding whether each request
   can reuse a connection from the pool. Though that can be avoided by using a
   DNS cache, it seems like the pool should short-circuit that entire process.
   So, this becomes some kind of scheme-host-port triplet. *)
(* TODO Also, how should this work with HTTP/2 and multiplexing? Need to address
   that next. *)
(* TODO The scheme is probably not sufficient. Will also need the negotiated
   protocol, as an https connection might have been upgraded to HTTP/2 or not at
   the server's discretion during ALPN. *)

(* TODO Implementation of pipelining might make it worthwhile to be able to tell
   the client when a request has completed sending (only). However, given
   pipelining is buggy and there is HTTP/2, maybe it's not worth complicating
   the API for this. *)

type create_result = {
  connection : connection;
  destroy : connection -> unit promise;
  concurrency : [ `Single_use | `Sequence | `Pipeline | `Multiplex ];
}
type create = endpoint -> create_result promise

type connection_pool = {
  obtain : endpoint -> request -> create -> (connection * int64) promise;
  write_done : int64 -> unit;
  all_done : int64 -> response -> unit;
  error : int64 -> unit;
}
(* TODO Return needs to provide a function for destroying a connection. *)

let connection_pool ~obtain ~write_done ~all_done ~error =
  {obtain; write_done; all_done; error}



type pooled_connection = {
  create_result : create_result;
  id : int64;
  created_at : float;
  mutable state : [
    | `Writing_request
    | `Reading_response_only
    | `Idle
  ];
  mutable ref_count : int;
  mutable idle_since : float;
  mutable closing : bool;
}

(* TODO Add various interesting limits. *)
let general_connection_pool () =
  let connections_by_id = Hashtbl.create 32
  and connections_by_endpoint = Hashtbl.create 32
  and next_id = ref 0L in

  let obtain endpoint _request create =
    (* TODO There are, in general, multiple connections for each endpoit, so,
       properly, the pool would have to either iterate over a list, or have an
       acceleration data structure for accessing ready connections by endpoint
       directly. *)
    (* TODO Must also check whether the connection is closing. However, the
       current pool never closes connections (!!!). *)
    (* TODO Also should respect connection concurrency. Sequential connections
       require the state to be `Idle. Pipeline connections require the state to
       be not `Writing_request. Multiplexing connections can be in any state to
       be reused. This code is currently hardcoded to do pipelining, which will
       conservatively work on HTTP/2 multiplexing, it just won't take advantage
       of the full concurrency available. *)
    match Hashtbl.find_opt connections_by_endpoint endpoint with
    | Some pooled_connection
        when pooled_connection.state <> `Writing_request ->
      pooled_connection.state <- `Writing_request;
      pooled_connection.ref_count <- pooled_connection.ref_count + 1;
      let connection = pooled_connection.create_result.connection
      and id = pooled_connection.id in
      Lwt.return (connection, id)
    | _ ->
      let%lwt create_result = create endpoint in
      let id = !next_id in
      next_id := Int64.succ !next_id;
      let pooled_connection = {
        create_result;
        id;
        created_at = Unix.time ();
        state = `Writing_request;
        ref_count = 1;
        idle_since = 0.;
        closing = false;
      } in
      Hashtbl.replace connections_by_id pooled_connection.id pooled_connection;
      Hashtbl.add connections_by_endpoint endpoint pooled_connection;
      Lwt.return (create_result.connection, pooled_connection.id)

  and write_done id =
    match Hashtbl.find_opt connections_by_id id with
    | None ->
      ()
    | Some pooled_connection ->
      pooled_connection.state <- `Reading_response_only
      (* TODO In a future version where other writers may be queued, this should
         wake up the head writer in the queue. *)

  and all_done id _response =
    match Hashtbl.find_opt connections_by_id id with
    | None ->
      ()
    | Some pooled_connection ->
      pooled_connection.ref_count <- pooled_connection.ref_count - 1;
      if pooled_connection.ref_count = 0 then begin
        pooled_connection.state <- `Idle;
        pooled_connection.idle_since <- Unix.time ()
      end

  and error =
    ignore
    (* TODO Definitely not correct - this should put the connection into the
       closing state and decrement its ref count. If the connection becomes idle
       because of that, it can be closed right away. *)

  in

  connection_pool ~obtain ~write_done ~all_done ~error



let default_connection_pool =
  lazy (general_connection_pool ())



(* TODO How should the host and port be represented? *)
(* TODO Good error handling. *)
(* TODO Probably change the default to one per-process pool with some
   configuration. *)
let send_one_request connection_pool hyper_request =
  let uri = Uri.of_string (Message.target hyper_request) in
  let scheme = Uri.scheme uri |> Option.get
  and host = Uri.host uri |> Option.get
  and port = Uri.port uri |> Option.value ~default:80
  (* and path_and_query = Uri.path_and_query uri *)
  in
  (* TODO Usage of Option.get above is temporary, though failure to provide a
     host should probably be a logic error, and doesn't have to be reported in a
     "neat" way - just a debuggable way. The port can be inferred from the
     scheme if it is missing. We are assuming http:// for now. *)

  (* TODO These sorts of things can probably be done by passing the client
     modules in as first-class modules. The code might be not so clear to read,
     though. *)
  let destroy connection =
    match connection with
    | Cleartext connection -> Httpaf_lwt_unix.Client.shutdown connection
    | SSL connection -> Httpaf_lwt_unix.Client.SSL.shutdown connection
    | H2 connection -> H2_lwt_unix.Client.SSL.shutdown connection
    (* | WebSocket stream -> Stream.close stream 1000; Lwt.return_unit *)
  in

  let create (scheme, host, port) =
    let%lwt addresses =
      Lwt_unix.getaddrinfo
        host (string_of_int port) [Unix.(AI_FAMILY PF_INET)] in
    let address = (List.hd addresses).Unix.ai_addr in
    (* TODO Note: this can raise. *)

    let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let%lwt () = Lwt_unix.connect socket address in

    match scheme with
    | "https" ->
      (* TODO The context needs to be created once per process, or a cache
          should be used. *)
      let context = Ssl.(create_context TLSv1_2 Client_context) in
      (* TODO For WebSockets (wss://), the client should probably do SSL
          without offering h2 by ALPN. Do any servers implement WebSockets over
          HTTP/2? *)
      Ssl.set_context_alpn_protos context ["h2"; "http/1.1"];
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
        Lwt.return {
          connection = H2 connection;
          destroy;
          concurrency = `Multiplex;
        }
      | _ ->
        let%lwt connection =
          Httpaf_lwt_unix.Client.SSL.create_connection ssl_socket in
        Lwt.return {
          connection = SSL connection;
          destroy;
          concurrency = `Pipeline;
        }
      end
      (* TODO Need to do server certificate validation here, etc. *)

    | "http" ->
      let%lwt connection = Httpaf_lwt_unix.Client.create_connection socket in
      Lwt.return {
        connection = Cleartext connection;
        destroy;
        concurrency = `Pipeline;
      }

    | "ws" ->
      (* TODO WebSocket support just needs to be completely refactored, and
         code common to the server and the client needs to be factored out. *)
      assert false
      (* TODO Need to return a different response depending on error conditions.
         websocket/af provides the server's response. *)
      (* let hyper_response =
        Message.response ~status:`OK

      (* TODO The weboscket/af client interface seems pretty awkward to use in
         this kind of control flow, since the input handlers need to be defined
         immediately. However, the input handlers themselves are ill-conceinved,
         since they are a partially push API (i.e. they lack full read flow
         control). So hack something together, and await a better API. *)
      let stream = ref None in
      (* TODO The equality between server and client input handlers is not
         exposed in the websocketaf API. *)
      let websocket_handler =
        Dream__http.Http.websocket_handler (fun the_stream ->
          stream := Some the_stream;
          Lwt.return_unit)
      in
      (* TODO Generate random nonces. *)
      let%lwt connection =
        Websocketaf_lwt_unix.Client.connect
          ~nonce:"abcdefghijklmnop"
          ~host
          ~port
          ~resource:path_and_query
          ~error_handler:ignore
          ~websocket_handler:(Obj.magic websocket_handler)
          socket
      in
      ignore connection;
      (* TODO Extremely questionable! The connection should just carry a stream
         promise instead, that the handler can wait on later. *)
      let%lwt () = Lwt_unix.sleep 1. in
      Lwt.return {
        connection = WebSocket (Option.get !stream);
        destroy;
        concurrency = `Single_use;
      } *)

    | _ ->
      assert false
      (* TODO Need a log and a more intelligent error here. *)
  in

  let endpoint = (scheme, host, port) in

  let%lwt (connection, id) =
    connection_pool.obtain endpoint hyper_request create in

  begin match connection with
  | Cleartext connection ->
    Hyper__http.Http1.http
      ~write_done:(fun () -> connection_pool.write_done id)
      ~all_done:(fun response -> connection_pool.all_done id response)
      connection
      hyper_request

  | SSL connection ->
    Hyper__http.Http1.https
      ~write_done:(fun () -> connection_pool.write_done id)
      ~all_done:(fun response -> connection_pool.all_done id response)
      connection
      hyper_request

  | H2 connection ->
    Hyper__http.Http2.https
      ~all_done:(fun response -> connection_pool.all_done id response)
      connection
      hyper_request

  (* | WebSocket _websocket ->
    assert false *)
    (* TODO Adapt and restore. *)
  end



(* TODO Add an option to redirect only to the same host? Or is this better
   addressed by just letting the user do redirects manually, if needed? It's
   probably best to expose some kind of filter function, because redirect
   handling is slightly tricky (with body streams), and the user can benefit by
   not having to write code themselves for this. *)
(* TODO Expose a redirect cache callback for permanent redirects. *)
(* TODO With mutable requests, it's probably better to allocate new requests
   after each redirect, so that the whole trace can be reported to the user. *)
let send ?(connection_pool = Lazy.force default_connection_pool) request =
  let rec redirect_loop remaining request =
    (* TODO Can save an allocation by binding the promise. *)
    let%lwt response = send_one_request connection_pool request in
    if remaining <= 0 then
      (* TODO Log a warning here if the original redirect limit was not zero. *)
      Lwt.return response
    else
      match Message.status response with
      | `Moved_Permanently
      | `Found
      | `See_Other
      | `Temporary_Redirect
      | `Permanent_Redirect ->
        begin match Message.header response "Location" with
        | None ->
          (* TODO Log a warning here. *)
          Lwt.return response
        | Some target ->
          (* TODO For Moved Permanently, Temporary Redirect, Permanent Redirect,
             warn if the server has read the request body, because we won't
             easily be able to resend it. *)
          (* TODO If requests become mutable, probably a new request should be
             explicitly allocated. *)
          (* TODO The URI in Location: might be absolute or not. *)
          Message.set_target request target;

          begin match Message.status response with
          | `Found
          | `See_Other ->
            Message.set_method_ request `GET
            (* TODO Note that doing this for 302 is not correct, but is done to
               match established behavior on the Web. *)
            (* TODO Should also substitute the body with an empty one here, and
               warn if the previous body is not closed (and close it). *)
            | _ ->
              ()
          end;

          redirect_loop (remaining - 1) request
        end
      | _ ->
        Lwt.return response
  in

  redirect_loop 5 request



(* TODO Which function should be the most fundamental function? Probably the
   request -> response runner. But it's probably not the most convenient for
   general usage.

   How should the host and port be represented? Can probably just allow them in
   [target], but also allow overriding them so that only the path and query are
   used. This could get confusing, though.

   To start with, implement a good request -> response runner that does the
   basics: create a request, allow streaming out its body, receive a response,
   allow streaming in its body. After that, elaborate. Probably should start
   with HTTP/2 and SSL.

   How are non-response errors reported? *)
