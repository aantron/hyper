<h1 align="center">Hyper</h1>

<p align="center">
Straightforward, full-featured Web client that composes with Dream.
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

*Note: the client is not actually full-featured yet :) This README is also an
extremely rough sketch :) This repository is a work in progress*

Hyper is the client counterpart to [Dream][dream], the Web server framework.
Hyper uses the same types and the same programming model. Like Dream, Hyper
offers one module, which features a bunch of easy-to-use functions. Also like
Dream, Hyper still exposes the underlying highly composable functions, for full
customization.

<br>

## Documentation

At the moment, Hyper offers only three main "stable" values:

```ocaml
Hyper.get : string -> string promise
Hyper.post : string -> string -> string promise
Hyper.websocket : string -> websocket promise
```

These support all the same Web protocols as Dream: HTTP/1, HTTP/2, TLS, and
WebSocket, and they follow redirects. The protocols are selected using URI
schemes:

```ocaml
Hyper.get "http://google.com"
Hyper.post "https://some.app/endpoint" "{}"
Hyper.websocket "ws://some.site/socket"
```

You can see early usage in the [examples][examples].

These are actually wrappers around a "stack" of `request -> response promise`
handlers and client-side middlewares, but the elements of that stack are not
mature enough to expose yet. They will eventually be exposed to quickly build
custom clients with any desired behavior.

For testing WebSockets, Hyper offers an interface similar to
[the one in Dream](https://aantron.github.io/dream/#websockets):

```ocaml
Hyper.send : websocket -> string -> unit promise
Hyper.receive : websocket -> string option promise
Hyper.close_websocket : websocket -> unit promise
```

[dream]: https://github.com/aantron/dream
[examples]: https://github.com/aantron/hyper/tree/master/example

<br>

## Composability

A Hyper client and a Dream server have exactly the same type,
`request -> response promise`.

This means that the client's lowest layer can be implemented by swapping out
connecting over the network by a direct call to an in-process Dream server,
which can be useful for no-network testing.

Conversely, a Dream server can pass the requests it receives to a Hyper client,
thus acting as a proxy. Responses received from the proxy client can be directly
returned by the server to *its* client.

In all cases, all body streams and WebSockets get forwarded automatically as a
side effect of the conventions followed by Hyper and Dream.

<br>

## Roadmap

- [ ] Restore [connection pooling and multiplexing](https://github.com/aantron/dream/blob/f69b95644a237be0aa3c9d3c6e29a7be32a5dbdb/src/hyper.ml#L76).
- [ ] 🛑 Server certificate validation.
- [ ] File uploads (multipart streams).
- [ ] Cookie store.
- [ ] Redirect cache.
- [ ] Automatic decompression.
- [ ] `wss://` (WebSockets over TLS).
- [ ] Many miscellania.
