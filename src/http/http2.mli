(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2021 Anton Bachin *)



module Message = Dream_pure.Message



val https :
  H2_lwt_unix.Client.SSL.t ->
    Message.request -> Message.response Message.promise
