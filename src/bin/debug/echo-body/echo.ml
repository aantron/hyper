let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [

    Dream.post "/echo" (fun request ->
      let%lwt body = Dream.body request in
      let content_type =
        match Dream.header request "Content-Type" with
        | None -> "text/plain; charset=utf-8"
        | Some v -> v
      in
      Dream.respond
        ~headers:["Content-Type", content_type]
        body);

  ]