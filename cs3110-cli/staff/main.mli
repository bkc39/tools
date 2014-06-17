val clean : unit -> unit
val build : string -> int
val doc : ?src_dir:string -> string -> int
val diff : string list -> unit
val email : unit -> unit
val test_parameterized : string -> string -> int
val test_logging_errors : string -> int
val test_quiet : string -> int
val test : string -> int
val assert_valid_line : unit -> string -> unit
val assert_valid_rubric : unit -> unit
val create_rubric : string list -> string list -> unit
val dict_of_rubric_file : string -> (string, int) Hashtbl.t
val reverse_create_rubric : string -> string list -> unit
val reverse_dict : string -> (string, bool * int) Hashtbl.t
val harness_collect_output : (string, int) Hashtbl.t -> int * string list
val harness : string -> string list -> unit
val run : string -> string list -> int
val reverse : string -> string -> string list -> unit
val smoke_compile_one : string list -> string -> unit
val smoke : string list -> unit
val help : unit -> unit
