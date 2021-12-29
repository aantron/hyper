let () =
  Lwt_main.run begin
    let request = Hyper.request "http://google.com" in
    let%lwt response = Hyper.send request in
    let%lwt body = Hyper.body response in
    print_string body;
    Lwt.return ()
  end



(* TODO Use the later higher-level wrappers. *)
