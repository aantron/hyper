<h1 align="center">Hyper</h1>

<p align="center">
Straightforward, full-featured Web client that works with Dream.
</p>

<br>

```ocaml
Lwt_main.run begin
  let%lwt response = Hyper.get "http://google.com" in
  print_string response;
  Lwt.return_unit
end
```

<br>

*Note: this repository is a work in progress. The client is not actually
full-featured yet :) This README is also an extremely rough sketch :)*

Hyper is the client counterpart to [Dream][dream], the Web server framework.
Hyper uses the same types and the same programming model. Like Dream, Hyper
offers one module, which features a bunch of easy-to-use functions. Also like
Dream, Hyper still exposes the underlying highly composable functions, for full
customization.

Hyper supports all the same Web protocols as Dream: HTTP/1, HTTP/2, TLS, and
WebSocket. You can see early usage in the [examples][examples].

[dream]: https://github.com/aantron/dream
[examples]: https://github.com/aantron/hyper/tree/master/example

<br>

## Roadmap

- [ ] Restore [connection pooling and multiplexing](https://github.com/aantron/dream/blob/f69b95644a237be0aa3c9d3c6e29a7be32a5dbdb/src/hyper.ml#L76).
- [ ] Server certificate validation.
- [ ] File uploads (multipart streams).
- [ ] Cookie store.
- [ ] Redirect cache.
- [ ] Automatic decompression.
- [ ] `wss://` (WebSockets over TLS).
- [ ] Miscellania.
