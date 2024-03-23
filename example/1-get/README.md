# `1-get`

<br>

This example shows the most basic usage: an HTTP `GET` request to `google.com`:

```ocaml
let () =
  Hyper.get "http://google.com"
  |> print_string
```

<br>

To build and run the example, do:

<pre><code><b>$ cd example/1-get</b>
<b>$ dune exec ./get.exe</b>
</code></pre>

If you go to [http://localhost:8080](http://localhost:8080), you will, of
course, see `Good morning, world!`. You can also try it in the [Dream
Playground](http://dream.as/1-hello).

<br>

<!--

**Next steps:**

- The next example, [**`2-middleware`**](../2-middleware#files), adds a logger
  to the app.
- [**`3-router`**](../3-router#files) sends requests to different handlers,
  depending on their path.

<br>

**See also:**

- [**`r-hello`**](../r-hello#files) is a Reason syntax version of this example.
- [**`w-esy`**](../w-esy#files) gives more detail on the [esy](https://esy.sh/)
  packaging.
- [**`w-watch`**](../w-watch#files) sets up a development watcher.


<br>

[Up to the tutorial index](../#readme)

-->
