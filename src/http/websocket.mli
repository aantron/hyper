(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



module Message = Dream_pure.Message



val ws :
  Lwt_unix.file_descr ->
    Message.request -> Message.response Message.promise
