(* This file is part of Hyper, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/hyper.

   Copyright 2022 Anton Bachin *)



module Message = Dream_pure.Message



let last_port = ref 8080

let next_port () =
  incr last_port;
  !last_port

let set_port request port =
  let uri = Message.target request |> Uri.of_string in
  Uri.with_port uri (Some port)
  |> Uri.to_string
  |> Message.set_target request



let with_tcp_responder port string f =
  Lwt_main.run begin
    let done_, signal_done = Lwt.wait () in

    let%lwt server =
      Lwt_io.establish_server_with_client_address
        Unix.(ADDR_INET (inet_addr_loopback, port))
        begin fun _client (_in, out) ->
          let%lwt () = Lwt_io.write out string in
          let%lwt () = Lwt_unix.sleep 0.1 in
          let%lwt () = Lwt_io.close out in
          Lwt.wakeup_later signal_done ();
          Lwt.return_unit
        end
    in

    Lwt.async f;

    let%lwt () = done_ in
    let%lwt () = Lwt_io.shutdown_server server in

    Lwt.return_unit
  end
