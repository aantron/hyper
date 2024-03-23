
type color_scheme = {
  error: Format.stag;
  info: Format.stag;
  key: Format.stag;
  string_value: Format.stag;
  header_key: Format.stag;
}

val print_json_content : ?colors:color_scheme -> string -> Format.formatter -> unit