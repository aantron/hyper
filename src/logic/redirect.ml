(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



module Message = Dream_pure.Message



(* TODO Store the redirect trace in a variable? *)
(* TODO Add an option to redirect only to the same host? Or is this better
   addressed by just letting the user do redirects manually, if needed? It's
   probably best to expose some kind of filter function, because redirect
   handling is slightly tricky (with body streams), and the user can benefit by
   not having to write code themselves for this. *)
(* TODO Expose a redirect cache callback for permanent redirects. *)
(* TODO With mutable requests, it's probably better to allocate new requests
   after each redirect, so that the whole trace can be reported to the user. Or
   the trace can consist of just the targets. *)
let follow_redirect ?(redirect_limit = 5) inner_handler request =
  let rec redirect_loop remaining request =
    let%lwt response = inner_handler request in
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
        | Some new_target ->
          (* TODO For Moved Permanently, Temporary Redirect, Permanent Redirect,
             warn if the server has read the request body, because we won't
             easily be able to resend it. *)
          (* TODO If requests become mutable, probably a new request should be
             explicitly allocated. *)
          (* TODO The URI in Location: might be absolute or not. *)
          (* TODO There probably need to be complex rules for reconciling the
             URI, such as what scheme to use if the scheme is missing, etc. *)
          let new_uri = Uri.of_string new_target in
          let new_target =
            match Uri.host new_uri with
            | Some _ ->
              new_target
            | None ->
              let old_uri = Uri.of_string (Message.target request) in
              let x f a b = f b a in (* TODO Very crude *)
              new_uri
              |> x Uri.with_scheme (Uri.scheme old_uri)
              |> x Uri.with_host (Uri.host old_uri)
              |> x Uri.with_port (Uri.port old_uri)
              |> Uri.to_string
          in
          Message.set_target request new_target;

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

  redirect_loop redirect_limit request
