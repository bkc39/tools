val absolute_path : string -> string
val split : string -> char -> bool -> string * string
val lsplit : string -> char -> string * string
val rsplit : string -> char -> string * string
val strip_suffix : string -> string
val strip_trailing_slash : string -> string
val strip_trailing_slash_all : string list -> string list
val tag_of_path : string -> string
val assert_file_exists : string -> unit
val ensure_dir : string -> unit
val directories_of_list : string -> string list
val test_name_of_line : string -> string
val get_extension : string -> string option
val filter_by_extension : string -> string list -> string list
val get_files_with_extension : string -> string -> string list
val get_subdirectories : string -> string list
