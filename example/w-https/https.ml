let response =
  Lwt_main.run (Hyper.get "https://github.com" ~headers:["Host", "github.com"])

let () =
  print_string response
