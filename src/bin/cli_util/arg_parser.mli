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

val parse_command_line : string array -> int -> command -> command

val option_requirements : option_specification list