(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



module H2_lwt_unix = Dream_h2_lwt_unix.H2_lwt_unix

module Message = Dream_pure.Message



val https :
  H2_lwt_unix.Client.SSL.t ->
    Message.request -> Message.response Message.promise
