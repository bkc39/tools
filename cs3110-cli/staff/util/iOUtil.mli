val read_lines : in_channel -> string list
val csv_of_file : string -> string
val do_if_directory : string -> (string -> unit) -> string -> int -> unit
