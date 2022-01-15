(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2021 Anton Bachin *)



module Message = Dream_pure.Message



val http :
  Httpaf_lwt_unix.Client.t ->
    Message.request -> Message.response Message.promise

val https :
  Httpaf_lwt_unix.Client.SSL.t ->
    Message.request -> Message.response Message.promise
