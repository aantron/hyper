(* This file is part of Dream, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/dream.

   Copyright 2021 Anton Bachin *)



val http :
  write_done:(unit -> unit) ->
  all_done:(Dream.response -> unit) ->
  Httpaf_lwt_unix.Client.t ->
    Dream.request -> Dream.response Dream.promise

val https :
  write_done:(unit -> unit) ->
  all_done:(Dream.response -> unit) ->
  Httpaf_lwt_unix.Client.SSL.t ->
    Dream.request -> Dream.response Dream.promise
