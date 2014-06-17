val return_code_of_exit_status : Unix.process_status -> int
val run_process : string -> string list -> int
val check_code : int -> unit
val config_env : unit -> unit
