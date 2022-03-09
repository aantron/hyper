let () =
  Hyper.get "http://google.com"
  |> print_string
