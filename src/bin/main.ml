let manual = "
Hyper - a CLI, cURL-like tool for humans.

Positional Arguments:

These arguments come after any flags and in the order they are listed here.
Only URL is required.

METHOD
    The HTTP method to be used for the request (GET, POST, PUT, DELETE, ...).
    
    This argument can be omitted in which case Hyper will use POST if there
    is some data to be sent, otherwise GET:
    
        $ hyper example.org               # => GET
        $ hyper example.org hello=world   # => POST
    
URL
    The scheme defaults to 'http://' if the URL does not include one.
    (You can override this with: --default-scheme=https)
    
    You can also use a shorthand for localhost
    
        $ hyper //:3000                   # => http://localhost:3000
        $ hyper //:3000/foo               # => http://localhost:3000/foo
        $ hyper /foo                      # => http://localhost/foo
        $ hyper foo                       # => http://localhost/foo

    Incorrect usage examples. httpie handles it as port number.
        $ hyper :3000                     # => http://localhost/3000
      
REQUEST_ITEM
    Optional key-value pairs to be included in the request. The separator used
    determines the type:
    
    ':' HTTP headers:
    
        Referer:http://example.org  Cookie:foo=bar  User-Agent:hyper/1.0
    
    '==' URL parameters to be appended to the request URI:
    
        search==dream
    
    '=' Data fields to be serialized into a JSON object (with --json, -j)
        or form data (with --form, -f):
    
        name=Hyper  language=Ocaml  description='CLI HTTP client'
    
    ':=' Non-string JSON data fields (only with --json, -j):
    
        awesome:=true  amount:=42  colors:='[\"red\", \"green\", \"blue\"]'
    
    You can use a backslash to escape a colliding separator in the field name:
    
        field-name-with\\:colon=value

";

module Message = Dream_pure.Message
module Status = Dream_pure.Status
module Ansi = Terminal.Style
module ArgParser = Hyper__cli_util.Arg_parser

let initial_command : ArgParser.command = {
  options = {
    flags = [];
  };
  request = {
    url = String.empty;
    verb = String.empty;
    headers = [];
    url_parameters = [];
    data_fields = [];
  }
}

let colors : Hyper__cli_util.Message_printer.color_scheme = {
  error = Format.String_tag "red";
  info = Format.String_tag "green";
  key = Format.String_tag "blue";
  string_value = Format.String_tag "c6(3,2,0)";
  header_key = Format.String_tag "cyan";
}

let print_error ppf msg =
  Format.fprintf ppf "%aError%a: %s" Format.pp_open_stag colors.error Format.pp_close_stag () msg;
  Format.pp_print_newline ppf ();
  Format.fprintf ppf "%aUsage%a: hyper [OPTIONS] [METHOD] URL [REQUEST_ITEM [REQUEST_ITEM ...]]"
    Format.pp_open_stag colors.info
    Format.pp_close_stag ();
  Format.pp_print_newline ppf ();
  Format.pp_print_string ppf "Run 'hyper --help' to get detailed description.";
  Format.pp_print_newline ppf ()

let print_help ppf options =
  Format.fprintf ppf "%aUsage%a: hyper [OPTIONS] [METHOD] URL [REQUEST_ITEM [REQUEST_ITEM ...]]"
    Format.pp_open_stag colors.info
    Format.pp_close_stag ();
  Format.pp_print_newline ppf ();
  Format.pp_print_string ppf manual;
  Format.fprintf ppf "OPTIONS@,@;<1 2>@[<hv 0>";
  List.iter
    (fun (opt) -> match opt with
      | Hyper__cli_util.Arg_parser.Flag (long_key, short_key, _, description) ->
        Format.fprintf ppf "-%s" long_key;
        if String.length short_key > 0 then
          Format.fprintf ppf ", -%s" short_key;
        Format.fprintf ppf "@;<1 2>@[<hv 0>%t@]@,@," description;
      | Setting (long_key, short_key, key_value_name, _, description) ->
        Format.fprintf ppf "-%s %s" long_key key_value_name;
        if String.length short_key > 0 then
          Format.fprintf ppf ", -%s %s" short_key key_value_name;
        Format.fprintf ppf "@;<1 2>@[<hv 0>%t@]@,@," description;)
    options;
  Format.fprintf ppf "@]@."

let rec print_headers ppf headers =
  match headers with
  | [] -> Format.pp_print_newline ppf ()
  | (key, value)::t ->
    Format.fprintf ppf "%a%s%a: %s" Format.pp_open_stag colors.header_key key Format.pp_close_stag () value;
    Format.pp_print_newline ppf ();
    print_headers ppf t

let raise_response response =
  let%lwt () = Message.close (Message.client_stream response) in
  raise (Hyper.Response response)

let add_header (cmd:ArgParser.command) (headers:(string * string) list) =
  let unique_headers = List.filter (fun (h) -> List.mem h cmd.request.headers = false) headers in
  { cmd with request = { cmd.request with headers = unique_headers @ cmd.request.headers }}

let get_media_type = function
  | Some ct -> let mime_type = (ct |> (String.split_on_char ';') |> List.hd |> (String.split_on_char '/')) in
      Some (List.hd mime_type, List.nth mime_type 1)
  | None -> None

let is_output_redirected (cmd:ArgParser.command) =
  Unix.isatty Unix.stdout = false
  || List.mem ArgParser.DOWNLOAD cmd.options.flags

let get_filename_opt = function
  | Some cd -> let filenames = (cd
    |> (String.split_on_char ';')
    |> List.map (fun x -> String.trim x)
    |> List.find_all (fun x -> String.starts_with ~prefix:"filename" x)
    |> List.sort (String.compare)) in
    if List.length filenames > 0 then
      let filename = filenames |> List.hd |> (String.split_on_char '=') in
      match List.nth_opt filename 1 with
      | Some name -> (name |> (String.split_on_char '"') |> List.nth_opt) 1
      | None -> None
    else
      None
  | None -> None

let get_name_from_uri uri =
  let path_parts = uri |> Uri.path |> Uri.pct_decode |> (String.split_on_char '/') in
  let last_part = List.nth path_parts (List.length path_parts - 1) |> String.trim in
  if String.length last_part == 0 then
    Option.value (Uri.host uri) ~default:"hyper"
  else
    last_part

let get_output_file_name content_disposition_opt content_type_opt (uri:Uri.t) () =
  let filename_opt = get_filename_opt content_disposition_opt in
  let origin_filename = match filename_opt with
  | Some filename -> Filename.basename filename
  | None ->
    let name_from_uri = Filename.basename @@ get_name_from_uri uri in
    let extension = Filename.extension name_from_uri in
    if String.length extension > 0 then
      name_from_uri
    else
      let media_type = get_media_type content_type_opt in
      match media_type with
      | Some (_, ext) -> name_from_uri ^ "." ^ ext
      | None -> name_from_uri in
  let exists = ref (Sys.file_exists origin_filename) in
  let i = ref 0 in
  while !exists do
    i := !i + 1;
    exists := Sys.file_exists @@ Printf.sprintf "%s(%i)%s" (Filename.remove_extension origin_filename) !i (Filename.extension origin_filename);
  done;
  if !i > 0 then
    Printf.sprintf "%s(%i)%s" (Filename.remove_extension origin_filename) !i (Filename.extension origin_filename)
  else
    origin_filename

let get_output_channel (cmd:ArgParser.command) get_file_name =
  if Unix.isatty Unix.stdout = false then
    stdout
  else if List.mem ArgParser.DOWNLOAD cmd.options.flags then
    let flag_opt = List.find_opt (fun flag ->
      match flag with
      | ArgParser.OUTPUT _ -> true
      | _ -> false
    ) cmd.options.flags in
    match flag_opt with
    | Some ArgParser.OUTPUT name -> open_out name
    | Some _ -> raise (Failure "Flag matching error.")
    | None ->
        let name = get_file_name () in
        open_out name
  else
    stdout

let apply_color color s = Ansi.(code color) ^ s ^ Ansi.(code none)

let unlimited_bar min_interval =
  let open Progress in
  let open Progress.Line.Using_int64 in
  let frames =
    let width = 6 in
    List.init width (fun i ->
      String.concat ""
        (List.init width (fun x ->
          if x = i then apply_color (Ansi.fg @@ Color.ansi `cyan) ">"
          else apply_color Ansi.faint "░")))
  in
  let spin = spinner ~min_interval ~frames () in
  const "|" ++ spin ++ spin ++ spin ++ spin ++ spin ++ const "|" ++ bytes ++ bytes_per_sec

let bar_with_total total =
  let open Progress.Line.Using_int64 in
  let open Progress.Color in
  list
    [ const "↓"
    ; spinner ()
    ; brackets (elapsed ())
    ; bytes
    ; bar ~style:`UTF8 ~color:(hex "#446E9E") total
    ; percentage_of total
    ; bytes_per_sec
    ]

let () =
  let response_body_formatter = Format.std_formatter in
    Ocolor_format.prettify_formatter response_body_formatter;
  let err_formatter = Format.err_formatter in
      Ocolor_format.prettify_formatter err_formatter;

  try 
    let parsed_cmd = ArgParser.parse_command_line Sys.argv 1 initial_command in
    let formatter =
      if is_output_redirected parsed_cmd then
        Format.err_formatter
      else
        Format.std_formatter in
      Ocolor_format.prettify_formatter formatter;
    if List.mem ArgParser.HELP parsed_cmd.options.flags then
      print_help formatter ArgParser.option_requirements
    else
    let cmd_with_verb = if String.length parsed_cmd.request.verb == 0 then
      if List.length parsed_cmd.request.data_fields > 0 then
        { parsed_cmd with request = { parsed_cmd.request with verb = "POST" }}
      else { parsed_cmd with request = { parsed_cmd.request with verb = "GET" }}
    else parsed_cmd in
    let cmd = if List.length cmd_with_verb.request.data_fields > 0 then
      if List.mem ArgParser.FORM cmd_with_verb.options.flags then
          add_header cmd_with_verb [("Content-Type", "application/x-www-form-urlencoded")]
      else
          add_header cmd_with_verb [("Content-Type", "application/json"); ("Accept", "application/json")]
    else cmd_with_verb
    in

    Lwt_main.run begin
      let parsed_uri =  (cmd.request.url |> Uri.of_string |> Uri.with_query') (cmd.request.url_parameters |> List.rev) in

      if List.mem ArgParser.DEBUG cmd.options.flags then (
        Format.fprintf formatter "@{<green>String URI@}: %s%a" cmd.request.url Format.pp_print_newline ();
        Format.fprintf formatter "@{<green>Parsed URI@}: %a%a" Uri.pp parsed_uri Format.pp_print_newline ();
      );

      let target_uri =
        let uri_with_default_scheme =
          match Uri.scheme parsed_uri with
          | None
          | Some "" -> Uri.with_scheme parsed_uri (Some "http")
          | Some _ -> parsed_uri in
          match Uri.host uri_with_default_scheme with
          | None
          | Some "" -> Uri.with_host uri_with_default_scheme (Some "localhost")
          | Some _ -> uri_with_default_scheme in

      if List.mem ArgParser.DEBUG cmd.options.flags then (
        Format.fprintf formatter "@{<green>Target URI@}: %a%a" Uri.pp target_uri Format.pp_print_newline ();
        Format.pp_print_newline formatter ();
      );

      let rec pipe_stdin_to out_stream =
        (*Read data from stdin*)
        let length = 4096 in
        let buffer = Bytes.create length in
        let%lwt bytes_read = Lwt_io.read_into Lwt_io.stdin buffer 0 length in
        match bytes_read with
        | 0 -> Hyper.close out_stream
        | len ->
          let%lwt () = Hyper.write out_stream (Bytes.sub_string buffer 0 len) in
          let%lwt () = Hyper.flush out_stream in
          pipe_stdin_to out_stream
      in
      let request =
        if Unix.isatty Unix.stdin then
          (*Handle data fields. There are two types of them: string and non-string. First one may be
            serialized in two different way: as form data and as json. The second one only as json.*)
          let body = if List.length cmd.request.data_fields > 0 then
            if List.mem ArgParser.FORM cmd.options.flags then
              let data_fields = List.map (fun (d) ->
                match d with
                | ArgParser.StringData (key, value) -> (key, [value])
                | JsonData (key, value) -> (key, [value])
              ) cmd.request.data_fields in
              Some (Uri.encoded_of_query data_fields)
            else
              let json_data = `Assoc (List.map (fun (d) ->
                match d with
                | ArgParser.StringData (key, value) -> (key, `String value)
                | JsonData (key, value) ->
                  try (key, Yojson.Basic.from_string value)
                  with Yojson.Json_error _ -> raise (ArgParser.Invalid_request_item (Printf.sprintf "Item '%s' has invalid value '%s'" key value))
              ) cmd.request.data_fields) in
              Some (Yojson.Basic.to_string json_data)
          else None
          in
          Hyper.request
            ?method_:(Some (`Method cmd.request.verb))
            ?headers:(Some cmd.request.headers)
            ?body:body
            (Uri.to_string target_uri)
        else
          Hyper.stream
            ?method_:(Some (`Method cmd.request.verb))
            ?headers:(Some cmd.request.headers)
            (Uri.to_string target_uri)
            (fun (request_stream) -> pipe_stdin_to request_stream)
      in
      if List.mem ArgParser.VERBOSE cmd.options.flags then (
        (*TODO: Print request*)
      );

      let%lwt response = Hyper.run request in
      let response_status = Message.status response in
      (*Print response status*)
      Format.fprintf formatter "%aHTTP%a/%a1.1 %i%a %a%s%a"
        Format.pp_open_stag colors.key Format.pp_close_stag ()
        (*TODO: Highlight error responses using red color*)
        Format.pp_open_stag colors.key (Status.status_to_int response_status) Format.pp_close_stag ()
        Format.pp_open_stag colors.header_key (Status.status_to_string response_status) Format.pp_close_stag ();
      Format.pp_print_newline formatter ();
      let headers = Message.all_headers response in
      print_headers formatter headers;

      let content_type = Message.header response "Content-Type" in
      if is_output_redirected cmd then (
        let get_file_name = get_output_file_name (Message.header response "Content-Disposition") content_type target_uri in
        let content_out_channel = get_output_channel cmd get_file_name in
        let content_length = match Message.header response "Content-Length" with
        | None -> None
        | Some str_value -> Int64.of_string_opt str_value in
        let open Progress in
        let line = match content_length with
        | None -> unlimited_bar (Some (Duration.of_int_ms 80))
        | Some total -> bar_with_total total
        in
        with_reporter (line) (fun (report) ->
          let response_stream = Hyper.body_stream response in
          let rec loop count =
            match%lwt Hyper.read response_stream with
            | None ->
              close_out content_out_channel;
              Hyper.close response_stream
            | Some chunk ->
              Progress.interject_with (fun () ->
                Printf.fprintf content_out_channel "%s" chunk
              );
              chunk |> String.length |> Int64.of_int |> report;
              (*TODO: It's for debug purpose. Remove it.*)
              Unix.sleepf 0.01;
              loop (Int64.add count 1L)
          in
          loop 1L
        )
      ) else (
        (*Format and prettify response depending on its type*)
        let media_type = get_media_type content_type in
        let%lwt content = match media_type with
        | Some ("text", _) | Some ("application", "json") -> let%lwt body = Hyper.body response in Lwt.return(Some body)
        | _ -> Lwt.return_none
        in
        match content with
        | Some body -> (
          match media_type with
          | Some ("application", "json") -> Hyper__cli_util.Message_printer.print_json_content ~colors:colors body response_body_formatter;
            Lwt.return_unit
          | _ ->
            (*TODO: Prettify other content type*)
            Format.pp_print_string response_body_formatter body;
            Format.pp_print_newline response_body_formatter ();
            Format.pp_print_flush response_body_formatter ();
            Lwt.return_unit
        )
        | None ->
            Format.pp_print_string formatter "+-----------------------------------------+";
            Format.pp_print_newline formatter ();
            Format.pp_print_string formatter "| NOTE: binary data not shown in terminal |";
            Format.pp_print_newline formatter ();
            Format.pp_print_string formatter "+-----------------------------------------+";
            Format.pp_print_newline formatter ();
            Format.pp_print_flush formatter ();
            Lwt.return_unit
      )
    end
  with
  | ArgParser.Invalid_command_argument msg -> print_error err_formatter msg
  | ArgParser.Invalid_request_item msg -> print_error err_formatter msg