open Core.Std
open FilepathUtil
open ProcessUtil

let run_command =
  Command.basic
    ~summary:"Runs an OCaml file."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "run <filename> <args> command runs the compiled bytecode executable for";
      "<filename> on the arguments <args>. Remember to compile <filename>";
      "with 'cs3110 compile <filename>' before using this command."
     ])
    Command.Spec.(
    empty
    +> flag "-native" no_arg ~doc:"Compile natively compile files."
    +> flag
         "-look-in"
         (required file)
         ~doc:"Directory to the search for the compiled file. Defaults to '_build'"
    +> anon ("filename" %: file)
    +> anon (sequence ("args" %: string)))
    (fun run_native look_in filename args () ->
     let cmd = if run_native
               then Format.sprintf "%s/%s.d.native" look_in filename
               else Format.sprintf "%s/%s.d.byte"   look_in filename in
     assert_file_exists cmd;
     ignore (run_process cmd args))

(* lol. questionable name-scheme... *)
let run_run () =
  Command.run ~version:"2.0" ~build_info:"Core" run_command
