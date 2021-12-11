(* This file is part of Dream, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/dream.

   Copyright 2021 Anton Bachin *)



val https :
  all_done:(Dream.response -> unit) ->
  H2_lwt_unix.Client.SSL.t ->
    Dream.request -> Dream.response Dream.promise
