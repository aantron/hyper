(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



module Message = Dream_pure.Message



let set_host_header inner_handler request =
  let uri = Uri.of_string (Message.target request) in
  let host = Uri.host_with_default ~default:"" uri in
  let host =
    match Uri.port uri with
    | None -> host
    | Some port -> host ^ ":" ^ (string_of_int port)
  in
  Message.set_header request "Host" host;
  inner_handler request

let set_user_agent_header inner_handler request =
  if not (Message.has_header request "User-Agent") then
    Message.set_header request "User-Agent" "hyper";
  inner_handler request
