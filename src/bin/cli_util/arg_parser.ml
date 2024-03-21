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
  | DEBUG
  | VERBOSE
  | DOWNLOAD
  | OUTPUT of string
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

type option_specification =
  | Flag of string * string * flag * (Format.formatter -> unit)
  | Setting of string * string * string * (string array -> int -> flag * int) * (Format.formatter -> unit)

let get_next_arg argv pos = 
  let next_arg = argv.(pos) in (next_arg, pos + 1)

let option_requirements : option_specification list = [
  Flag ("-debug", "", DEBUG, (fun (ppf) -> Format.fprintf ppf
    "Print debug information."));
  Flag ("-json", "j", JSON, (fun (ppf) -> Format.fprintf ppf
    "(default) Data items from the command line are serialized as a JSON object.@,\
     The Content-Type and Accept headers are set to application/json (if not specified)."));
  Flag ("-form", "f", FORM, (fun (ppf) -> Format.fprintf ppf
    "Data items from the command line are serialized as form fields.@,\
     The Content-Type is set to application/x-www-form-urlencoded (if not specified).@ \
     The presence of any file fields results in a multipart/form-data request."));
  Flag ("-verbose", "v", VERBOSE, (fun (ppf) -> Format.fprintf ppf
    "Verbose output. Print the whole request as well as the response."));
  Flag ("-help", "", HELP, (fun (ppf) -> Format.fprintf ppf
    "Show this help message and exit."));
  Flag ("-download", "d", DOWNLOAD, (fun (ppf) -> Format.fprintf ppf
    "Do not print the response body to stdout. Rather, download it and store it in a file."));
  Setting ("-output", "o", "FILE",
    (fun argv pos -> match get_next_arg argv pos with
      | (file_name, next_pos) -> (OUTPUT (file_name), next_pos)
      | exception Invalid_argument _ -> raise (Invalid_command_argument "--output option requires FILE argument")),
    (fun (ppf) -> Format.fprintf ppf
      "Save output to FILE instead of stdout. If --download is also set, then only@ \
       the response body is saved to FILE. Other parts of the HTTP exchange are@ \
       printed to stderr."));
]

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
  match List.find
    (fun (opt) -> match opt with
      | Flag (long_key, short_key, _, _) | Setting (long_key, short_key, _, _, _) ->
        long_key = option_key || short_key = option_key)
    option_requirements with
  | Flag (_, _, flag, _) ->
    parse_command_line argv (start + 1) { command with options = { flags = flag :: command.options.flags }}
  | Setting (_, _, _, prod_fun, _) ->
    let flag, next_pos = prod_fun argv start in
    parse_command_line argv (next_pos + 1) { command with options = { flags = flag :: command.options.flags }}
  | exception Not_found -> raise (Invalid_command_argument (Printf.sprintf "Unsupported command option: '-%s'" option_key))

and parse_url argv start command =
  if String.length command.request.url != 0
  then command
  else
  let next = start + 1 in
  let r = Str.regexp "[A-Z]+" in (*regular expression testing whether the given string is HTTP method*)
  if (Array.length argv) - start <= 1
    || String.starts_with ~prefix:"-" argv.(next)
    || Str.string_match r argv.(start) 0 = false
    || String.length command.request.verb != 0
  then parse_command_line argv next { command with request = { command.request with url = argv.(start) }}
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
