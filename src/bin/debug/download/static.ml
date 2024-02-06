let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/download/**" (Dream.static "./files")
  ]
