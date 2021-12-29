(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2021 Anton Bachin *)



module Message = Dream_pure.Message
(* TODO Hide this and all mentions of it from the generated docs. *)



type request = Message.request
type response = Message.response
type 'a promise = 'a Lwt.t



type connection_pool

val send :
  ?connection_pool:connection_pool ->
    request -> response promise



type connection
type endpoint
type create_result = {
  connection : connection;
  destroy : connection -> unit promise;
  concurrency : [ `Single_use | `Sequence | `Pipeline | `Multiplex ];
}
type create = endpoint -> create_result promise

val connection_pool :
  obtain:(endpoint -> request -> create -> (connection * int64) promise) ->
  write_done:(int64 -> unit) ->
  all_done:(int64 -> response -> unit) ->
  error:(int64 -> unit) ->
    connection_pool
