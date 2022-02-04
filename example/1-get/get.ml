let response =
  Lwt_main.run (Hyper.get "http://google.com")

let () =
  print_string response
