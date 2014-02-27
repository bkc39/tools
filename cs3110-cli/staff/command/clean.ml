open Core.Std
open ProcessUtil

(** [clean ()] removes all files generated during compilation. *)
let clean () : unit =
  check_code (Sys.command "ocamlbuild -clean")

let clean_command =
  Command.basic
    ~summary:"Removes all of the files generated durign compilation."
    ~readme:(fun () ->
      "The clean command is a wrapper for the clean utility of corebuild.")
    Command.Spec.empty 
    clean

let run_clean_command () =
  Command.run ~version:"2.0" ~build_info:"Core" clean_command
