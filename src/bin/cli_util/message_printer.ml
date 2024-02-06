
type color_scheme = {
  error: Format.stag;
  info: Format.stag;
  key: Format.stag;
  string_value: Format.stag;
  header_key: Format.stag;
}

let base_colors = {
  error = Format.String_tag "red";
  info = Format.String_tag "green";
  key = Format.String_tag "blue";
  string_value = Format.String_tag "c6(3,2,0)";
  header_key = Format.String_tag "cyan";
}

let rec print_json ?(colors = base_colors) ppf json deep =
  match json with
  | `Null -> "null" |> Format.pp_print_string ppf
  | `Bool value -> Bool.to_string value |> Format.pp_print_string ppf
  | `Int value -> Int.to_string value |> Format.pp_print_string ppf
  | `Float value -> Float.to_string value |> Format.pp_print_string ppf
  | `String value -> Format.fprintf ppf "%a\"%s\"%a" Format.pp_open_stag colors.string_value (String.escaped value) Format.pp_close_stag ()
  | `Assoc obj ->
    Format.pp_open_box ppf 0;
    Format.fprintf ppf "{";
    print_json_key_value ~colors ppf obj deep
  | `List v ->
    Format.pp_open_box ppf 0;
    Format.pp_print_string ppf "[";
    print_json_array ~colors ppf v deep
and print_json_key_value ?(colors = base_colors) ppf obj deep =
  match obj with
  | [] ->
    Format.pp_print_break ppf 1 0;
    Format.pp_print_string ppf "}";
    Format.pp_close_box ppf ()
  | (key, json_value)::t ->
    Format.pp_print_break ppf 1 deep;
    Format.fprintf ppf "%a\"%s\"%a: " Format.pp_open_stag colors.key (String.escaped key) Format.pp_close_stag ();
    print_json ~colors ppf json_value deep;
    if List.length t > 0 then Format.fprintf ppf ",";
    print_json_key_value ~colors ppf t deep
and print_json_array ?(colors = base_colors) ppf v deep =
  match v with
  | [] ->
    Format.pp_print_break ppf 1 0;
    Format.pp_print_string ppf "]";
    Format.pp_close_box ppf ()
  | value::t ->
    Format.pp_print_break ppf 1 deep;
    print_json ~colors ppf value deep;
    if List.length t > 0 then Format.fprintf ppf ",";
    print_json_array ~colors ppf t deep

let print_json_content ?(colors = base_colors) content formatter = 
  let json_response = Yojson.Basic.from_string content in
  Format.fprintf formatter "@[";
  print_json ~colors formatter json_response 2;
  Format.fprintf formatter "@]";
  Format.pp_print_newline formatter ()