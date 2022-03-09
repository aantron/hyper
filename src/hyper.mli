(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



(** {1 Types} *)

type request = client message
and response = server message
and handler = request -> response promise
and middleware = handler -> handler

(** {2 Helpers} *)

and 'a message = 'a Dream_pure.Message.message
and client = Dream_pure.Message.client
and server = Dream_pure.Message.server
and 'a promise = 'a Lwt.t



(** {1 Quick use} *)

(* TODO Make these non-promise-valued. *)
exception Response of response

val get :
  ?headers:(string * string) list ->
  ?redirect_limit:int ->
  ?server:handler ->
    string -> string promise

val post :
  ?headers:(string * string) list ->
  ?redirect_limit:int ->
  ?server:handler ->
    string -> string -> string promise



(** {1 Methods} *)

type method_ = [
  | `GET
  | `POST
  | `PUT
  | `DELETE
  | `HEAD
  | `CONNECT
  | `OPTIONS
  | `TRACE
  | `PATCH
  | `Method of string
]

val method_to_string : [< method_ ] -> string
val string_to_method : string -> method_
val methods_equal : [< method_ ] -> [< method_ ] -> bool
val normalize_method : [< method_ ] -> method_



(** {1 Status codes} *)

type informational = [
  | `Continue
  | `Switching_Protocols
]

type successful = [
  | `OK
  | `Created
  | `Accepted
  | `Non_Authoritative_Information
  | `No_Content
  | `Reset_Content
  | `Partial_Content
]

type redirection = [
  | `Multiple_Choices
  | `Moved_Permanently
  | `Found
  | `See_Other
  | `Not_Modified
  | `Temporary_Redirect
  | `Permanent_Redirect
]

type client_error = [
  | `Bad_Request
  | `Unauthorized
  | `Payment_Required
  | `Forbidden
  | `Not_Found
  | `Method_Not_Allowed
  | `Not_Acceptable
  | `Proxy_Authentication_Required
  | `Request_Timeout
  | `Conflict
  | `Gone
  | `Length_Required
  | `Precondition_Failed
  | `Payload_Too_Large
  | `URI_Too_Long
  | `Unsupported_Media_Type
  | `Range_Not_Satisfiable
  | `Expectation_Failed
  | `Misdirected_Request
  | `Too_Early
  | `Upgrade_Required
  | `Precondition_Required
  | `Too_Many_Requests
  | `Request_Header_Fields_Too_Large
  | `Unavailable_For_Legal_Reasons
]

type server_error = [
  | `Internal_Server_Error
  | `Not_Implemented
  | `Bad_Gateway
  | `Service_Unavailable
  | `Gateway_Timeout
  | `HTTP_Version_Not_Supported
]

type standard_status = [
  | informational
  | successful
  | redirection
  | client_error
  | server_error
]

type status = [
  | standard_status
  | `Status of int
]

val status_to_string : [< status ] -> string
val status_to_reason : [< status ] -> string option
val status_to_int : [< status ] -> int
val int_to_status : int -> status
val is_informational : [< status ] -> bool
val is_successful : [< status ] -> bool
val is_redirection : [< status ] -> bool
val is_client_error : [< status ] -> bool
val is_server_error : [< status ] -> bool
val status_codes_equal : [< status ] -> [< status ] -> bool
val normalize_status : [< status ] -> status



(** {1 Requests} *)

val request :
  ?method_:[< method_ ] ->
  ?headers:(string * string) list ->
  ?body:string ->
  string ->
    request

val run :
  ?redirect_limit:int ->
  ?server:handler ->
    request -> response promise



(** {1 Responses} *)

val status : response -> status
val body : 'a message -> string promise



(** {1 Headers} *)

val header : 'a message -> string -> string option
val headers : 'a message -> string -> string list
val all_headers : 'a message -> (string * string) list
val has_header : 'a message -> string -> bool
val add_header : 'a message -> string -> string -> unit
val drop_header : 'a message -> string -> unit
val set_header : 'a message -> string -> string -> unit



(** {1 Streams} *)

type stream = Dream_pure.Stream.stream

val stream :
  ?method_:[< method_ ] ->
  ?headers:(string * string) list ->
  ?close:bool ->
  string ->
  (stream -> unit promise) ->
    request

val body_stream : response -> stream

val read : stream -> string option promise
val write : stream -> string -> unit promise
val flush : stream -> unit promise
val close : stream -> unit promise



(** {1 WebSockets} *)

type websocket

(* TODO Restore ?headers. *)
(* TODO Either build in the callback, or provide a with_websocket. *)
val websocket :
  (* ?headers:(string * string) list -> *)
  ?redirect_limit:int ->
  ?server:handler ->
    string -> websocket promise

type text_or_binary = [ `Text | `Binary ]
type end_of_message = [ `End_of_message | `Continues ]

val send :
  ?text_or_binary:[< text_or_binary ] ->
  ?end_of_message:[< end_of_message ] ->
    websocket -> string -> unit promise
val receive : websocket -> string option promise
val receive_fragment :
  websocket -> (string * text_or_binary * end_of_message) option promise
val close_websocket : ?code:int -> websocket -> unit promise



(** {1 Web formats} *)

val to_form_urlencoded : (string * string) list -> string



(* TODO Middleware helpers, once they are settled. *)
(* TODO Cookie stores. *)
(* TODO More Web format helpers. *)
(* TODO Expose the internal middleware stack so it can be re-composed for
   custom clients. *)
