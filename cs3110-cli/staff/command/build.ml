open Constants
open Core.Std
open IOUtil
open FilepathUtil
open ProcessUtil

(** [build] compile [m] into a bytecode executable. The
[with_exit_code] argument denotes what the program should do with the
exit code of [ocamlbuild]. The toplevel command takes [with_exit_code
= ProcessUtil.check_code]. Relies on [ocamlbuild]. *)
let build ?with_exit_code:(with_exit_code = check_code) run_quiet main_module () : unit =
  assert_file_exists (main_module ^ ".ml");
  let target = Format.sprintf "%s.d.byte" main_module in
  let _ = Format.printf "Compiling '%s.ml'\n%!" main_module in
  let dependencies = match Sys.file_exists depend_file with
    | `No  | `Unknown -> []
    | `Yes            -> ["-Is"; csv_of_file depend_file] in
  let libraries = match Sys.file_exists lib_file with
    | `No  | `Unknown -> ["-libs"; "assertions"]
    | `Yes            -> ["-libs"; "assertions," ^ csv_of_file lib_file] in
  let all_opam_packages = std_opam_packages @
    match Sys.file_exists opam_packages_file with
    | `No  | `Unknown -> []
    | `Yes            -> read_lines (open_in opam_packages_file) in
  let opam_packages_str =
    String.concat ~sep:", "
                  (List.map all_opam_packages
                            ~f:(fun p -> Format.sprintf "package(%s)" p)) in
  let ocamlbuild_flags =  [
    "-cflag";
    "-warn-error";
    "-cflag";
    "+a";                             (* treat the default warnings as errors *)
    "-use-ocamlfind";
    "-no-links";
    "-tag-line"; "<*.ml{,i}> : syntax(camlp4o), " ^ opam_packages_str;
    "-tag-line"; "<*.d.byte> : " ^ opam_packages_str;
    "-tag-line"; "<*.native> : " ^ opam_packages_str;
    target
  ] in
  with_exit_code (run_process "ocamlbuild" (
    dependencies @
    libraries    @
    if run_quiet then "-quiet"::ocamlbuild_flags else ocamlbuild_flags))

let exit_code_of_build main_module =
  let exit_code = ref None in
  build ~with_exit_code:(fun code -> exit_code := Some code) true main_module ();
  match !exit_code with
  | Some code -> code
  | None      -> begin
    let err_msg =
      Printf.sprintf "The build of '%s' failed to return an exit code"
                     main_module in
    failwith err_msg
  end

let build_command =
  Command.basic
    ~summary:"Compiles a file into a bytecode executable. Relies on ocamlbuild."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "The build command is a wrapper for the ocamlbuild tool. It takes the";
      "input file, resolves it's dependencies and compiles to a bytecode";
      "executable file. The object files produced during compilation are placed";
      "in a directory [_build], which will be created, if not already present,";
      "in the current working directory."
    ])
    Command.Spec.(
      empty
      +> flag "-q" no_arg ~doc:"Run quietly."
      +> anon ("filename" %: file))
    build

let run_build () =
  Command.run ~version:"2.0" ~build_info:"Core" build_command
