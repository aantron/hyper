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
    
        $ hyper :3000                    # => http://localhost:3000
        $ hyper :/foo                    # => http://localhost/foo
      
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

exception Invalid_command_argument of string
(*Raised when the given command line arguments are incorrect.*)
exception Invalid_request_item of string
(*Raised when the given request item is incorrect.*)

type data_field =
  | StringData of string * string
  | JsonData of string * string

type request_specification = {
  url: string;
  verb: string;
  headers: (string * string) list;
  url_parameters: (string * string) list;
  data_fields: data_field list;
}

type flag =
  | VERBOSE
  | JSON
  | FORM
  | HELP

type execution_options = {
  flags: flag list;
}

type command = {
  options: execution_options;
  request: request_specification;
}

let initial_command = {
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

type option_specification =
  | Flag of string * string * (unit -> flag) * (Format.formatter -> unit)
  
let option_requirements : option_specification list = [
  Flag ("-json", "j", (fun () -> JSON), (fun (ppf) -> Format.fprintf ppf "(default) Data items from the command line are serialized as a JSON object.@,The Content-Type and Accept headers are set to application/json (if not specified)."));
  Flag ("-form", "f", (fun () -> FORM), (fun (ppf) -> Format.fprintf ppf "Data items from the command line are serialized as form fields.@,The Content-Type is set to application/x-www-form-urlencoded (if not specified).@ The presence of any file fields results in a multipart/form-data request."));
  Flag ("-verbose", "v", (fun () -> VERBOSE), (fun (ppf) -> Format.fprintf ppf "Verbose output. Print the whole request as well as the response."));
  Flag ("-help", "", (fun () -> HELP), (fun (ppf) -> Format.fprintf ppf "Show this help message and exit."));
]

type color_scheme = {
  error: Format.stag;
  info: Format.stag;
  key: Format.stag;
  value: Format.stag;
  header_key: Format.stag;
}

let colors = {
  error = Format.String_tag "red";
  info = Format.String_tag "green";
  key = Format.String_tag "blue";
  value = Format.String_tag "yellow";
  header_key = Format.String_tag "cyan";
}

let rec parse_command_line argv start command =
  if List.mem HELP command.options.flags
  then command
  else if String.length command.request.url == 0 && (Array.length argv) - start <= 0
  then raise (Invalid_command_argument "the following arguments are required: URL")
  else if start >= Array.length argv then command
  else if String.starts_with ~prefix:"-" argv.(start)
  then parse_option argv start command
  else if String.length command.request.url == 0
  then parse_url argv start command
  else parse_request_item argv start command

and parse_option argv start command =
  let option_arg = argv.(start) in
  let option_key = String.sub option_arg 1 (String.length option_arg - 1) in
  match List.find (fun (Flag (long_key, short_key, _, _)) -> long_key = option_key || short_key = option_key) option_requirements with
  | Flag (_, _, prod_fun, _) ->
    parse_command_line argv (start + 1) { command with options = { flags = prod_fun () :: command.options.flags }}
  | exception Not_found -> raise (Invalid_command_argument (Printf.sprintf "Unsupported command option: '-%s'" option_key))

and parse_url argv start command =
  if String.length command.request.url != 0
  then command
  else
  let next = start + 1 in
  let r = Str.regexp "[A-Za-z]+" in (*regular expression testing whether the given string is HTTP method*)
  if (Array.length argv) - start <= 1
    || String.starts_with ~prefix:"-" argv.(next)
    || Str.string_match r argv.(start) 0 = false
    || String.length command.request.verb != 0
  then parse_command_line argv next { command with request = { command.request with url = "//" ^ argv.(start) }}
  else parse_url argv next { command with request = { command.request with verb = String.uppercase_ascii argv.(start) }}

and parse_request_item argv start command =
  let key_value_string = argv.(start) in
  let max_key_name_length = String.length key_value_string in
  let key_name_buf = Buffer.create max_key_name_length in
  let delimiter_buf = Buffer.create 3 in

  let found_delimiter = ref false in
  let escaped = ref false in
  let i = ref 0 in
  while !found_delimiter == false && !i < max_key_name_length do
    found_delimiter := !escaped == false
      && (key_value_string.[!i] == ':' || key_value_string.[!i] == '=');
    escaped := key_value_string.[!i] == '\\';
    if !found_delimiter == false && !escaped == false then
      Buffer.add_char key_name_buf key_value_string.[!i];
    if !found_delimiter == true then
      Buffer.add_char delimiter_buf key_value_string.[!i];
    i := !i + 1;
  done;
  let key_name = Buffer.contents key_name_buf in
  if !found_delimiter == false then raise (Invalid_request_item (Printf.sprintf "Item '%s' has invalid or no delimiter" key_name));
  if !i < max_key_name_length && key_value_string.[!i] == '='
  then (
    Buffer.add_char delimiter_buf key_value_string.[!i];
    i := !i + 1;
  );
  if !i >= max_key_name_length then raise (Invalid_request_item (Printf.sprintf "No value specified for the item '%s'" key_name));
  let delimiter = Buffer.contents delimiter_buf in
  let value = String.sub key_value_string !i (max_key_name_length - !i) in
  let command_with_item = match delimiter with
    | ":" -> { command with request = { command.request with headers = (key_name, value) :: command.request.headers }}
    | "==" -> { command with request = { command.request with url_parameters = (key_name, value) :: command.request.url_parameters }}
    | "=" -> { command with request = { command.request with data_fields = StringData (key_name, value) :: command.request.data_fields }}
    | ":=" -> { command with request = { command.request with data_fields = JsonData (key_name, value) :: command.request.data_fields }}
    | _ -> raise (Invalid_request_item (Printf.sprintf "Item '%s' has unsupported delimiter '%s'" key_name delimiter))
  in parse_command_line argv (start + 1) command_with_item 

let print_error ppf msg =
  Format.fprintf ppf "%aError%a: %s" Format.pp_open_stag colors.error Format.pp_close_stag () msg;
  Format.pp_print_newline ppf ();
  Format.fprintf ppf "%aUsage%a: hyper [OPTIONS] [METHOD] URL [REQUEST_ITEM [REQUEST_ITEM ...]]"
    Format.pp_open_stag colors.info
    Format.pp_close_stag ();
  Format.pp_print_newline ppf ();
  Format.pp_print_string ppf "Run 'hyper --help' to get detailed description.";
  Format.pp_print_newline ppf ()

let print_help ppf =
  Format.fprintf ppf "%aUsage%a: hyper [OPTIONS] [METHOD] URL [REQUEST_ITEM [REQUEST_ITEM ...]]"
    Format.pp_open_stag colors.info
    Format.pp_close_stag ();
  Format.pp_print_newline ppf ();
  Format.pp_print_string ppf manual;
  Format.fprintf ppf "OPTIONS@,@;<1 2>@[<hv 0>";
  List.iter (fun (Flag (long_key, short_key, _, description)) ->
    Format.fprintf ppf "-%s" long_key;
    if String.length short_key > 0 then
      Format.fprintf ppf ", -%s" short_key;
    Format.fprintf ppf "@;<1 2>@[<hv 0>%t@]@,@," description;
  ) option_requirements;
  Format.fprintf ppf "@]@."

let rec print_json ppf json deep =
  match json with
  | `Null -> "null" |> Format.pp_print_string ppf
  | `Bool value -> Bool.to_string value |> Format.pp_print_string ppf
  | `Int value -> Int.to_string value |> Format.pp_print_string ppf
  | `Float value -> Float.to_string value |> Format.pp_print_string ppf
  | `String value -> Format.fprintf ppf "%a\"%s\"%a" Format.pp_open_stag colors.value (String.escaped value) Format.pp_close_stag ()
  | `Assoc obj ->
    Format.fprintf ppf "{";
    print_json_key_value ppf obj deep;
  | `List v ->
    Format.pp_open_hovbox ppf 0;
    Format.pp_print_string ppf "[";
    print_json_array ppf v deep
and print_json_key_value ppf obj deep=
  match obj with
  | [] ->
    Format.pp_print_break ppf 1 deep;
    Format.pp_print_string ppf "}"
  | (key, json_value)::t ->
    let next_deep = deep + 2 in
    Format.pp_print_break ppf 1 next_deep;
    Format.fprintf ppf "%a\"%s\"%a: " Format.pp_open_stag colors.key (String.escaped key) Format.pp_close_stag ();
    print_json ppf json_value next_deep;
    if List.length t > 0 then Format.fprintf ppf ",";
    print_json_key_value ppf t deep
and print_json_array ppf v deep =
  match v with
  | [] ->
    Format.pp_print_break ppf 1 deep;
    Format.pp_print_string ppf "]";
    Format.pp_close_box ppf ()
  | value::t ->
    Format.pp_print_break ppf 1 2;
    print_json ppf value deep;
    if List.length t > 0 then Format.fprintf ppf ",";
    print_json_array ppf t deep

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

let add_header cmd headers =
  let unique_headers = List.filter (fun (h) -> List.mem h cmd.request.headers = false) headers in
  { cmd with request = { cmd.request with headers = unique_headers @ cmd.request.headers }}

let () =
  let formatter = Format.std_formatter in
  Ocolor_format.prettify_formatter formatter;
  try 
    let parsed_cmd = parse_command_line Sys.argv 1 initial_command in
    if List.mem HELP parsed_cmd.options.flags
    then print_help formatter
    else
    let cmd_with_verb = if String.length parsed_cmd.request.verb == 0
      then if List.length parsed_cmd.request.data_fields > 0
        then { parsed_cmd with request = { parsed_cmd.request with verb = "POST" }}
        else { parsed_cmd with request = { parsed_cmd.request with verb = "GET" }}
      else parsed_cmd
    in
    let cmd = if List.length cmd_with_verb.request.data_fields > 0
      then if List.mem FORM cmd_with_verb.options.flags
        then
          add_header cmd_with_verb [("Content-Type", "application/x-www-form-urlencoded")]
        else
          add_header cmd_with_verb [("Content-Type", "application/json"); ("Accept", "application/json")]
      else cmd_with_verb
    in
    Lwt_main.run begin
      let parsed_uri =  (cmd.request.url |> Uri.of_string |> Uri.with_query') (cmd.request.url_parameters |> List.rev) in
      if List.mem VERBOSE cmd.options.flags
        then Format.fprintf formatter "@{<green>Parsed URI@}: %a%a" Uri.pp parsed_uri Format.pp_print_newline ();
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
      if List.mem VERBOSE cmd.options.flags then (
        Format.fprintf formatter "@{<green>Target URI@}: %a%a" Uri.pp target_uri Format.pp_print_newline ();
        Format.pp_print_newline formatter ();
      );
      (*Handle data fields. There are two types of them: string and non-string. First one may be
        serialized in two different way: as form data and as json. The second one only as json.*)
      let body = if List.length cmd.request.data_fields > 0
        then if List.mem FORM cmd.options.flags
          then
            let data_fields = List.map (fun (d) ->
              match d with
              | StringData (key, value) -> (key, [value])
              | JsonData (key, value) -> (key, [value])
            ) cmd.request.data_fields in
            Some (Uri.encoded_of_query data_fields)
          else
            let json_data = `Assoc (List.map (fun (d) ->
              match d with
              | StringData (key, value) -> (key, `String value)
              | JsonData (key, value) ->
                try (key, Yojson.Basic.from_string value)
                with Yojson.Json_error _ -> raise (Invalid_request_item (Printf.sprintf "Item '%s' has invalid value '%s'" key value))
            ) cmd.request.data_fields) in
            Some (Yojson.Basic.to_string json_data)
        else None
      in
      let request = Hyper.request
        ?method_:(Some (`Method cmd.request.verb))
        ?headers:(Some cmd.request.headers)
        ?body:body
        (Uri.to_string target_uri)
      in
      if List.mem VERBOSE cmd.options.flags then (
        (*TODO: Print request*)
      );
      let%lwt response = Hyper.run request in
      let response_status = Message.status response in
      (*Print response status*)
      Format.fprintf formatter "%aHTTP%a/%a1.1 %i%a %a%s%a"
        Format.pp_open_stag colors.key Format.pp_close_stag ()
        Format.pp_open_stag colors.key (Status.status_to_int response_status) Format.pp_close_stag ()
        Format.pp_open_stag colors.header_key (Status.status_to_string response_status) Format.pp_close_stag ()
      ;
      Format.pp_print_newline formatter ();
      let headers = Message.all_headers response in
      print_headers formatter headers;
      let%lwt content =
        if response_status = `OK then
          Hyper.body response
        else
          (*TODO: Handle error responses and show them*)
          raise_response response
      in
      (*Format and prettify response depending on its type*)
      let content_type = Message.header response "Content-Type" in
      let () = match content_type with
      | Some "application/json" -> 
        let json_response = Yojson.Basic.from_string content in
        Format.fprintf formatter "@[<hv 0>";
        print_json formatter json_response 0;
        Format.fprintf formatter "@]";
        Format.pp_print_newline formatter ()
      (*TODO: Prettify other content type*)
      | _ -> Format.pp_print_string formatter content
      in
      Format.pp_print_newline formatter ();
      Format.pp_print_flush formatter ();
      Lwt.return_unit
    end
  with
  | Invalid_command_argument msg -> print_error formatter msg
  | Invalid_request_item msg -> print_error formatter msg