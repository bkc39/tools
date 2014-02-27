open Core.Std
open FilepathUtil
open ProcessUtil

let doc (output_dir : string) (maybe_src_dir : string option) () : unit =
  let src_dir = match maybe_src_dir with
    | None      -> Sys.getcwd ()
    | Some path -> path in
  Printf.printf "Generating documentation for directory: %s\n" src_dir;
  let build_dir =
    try match Sys.is_directory "_build" with
    | `Yes -> "_build"
    | `No  | `Unknown -> begin
      Printf.eprintf "Please run %s before generating the documentation."
        "\'cs3110 compile <main-module>\'";
      exit 1
    end
    with _ -> "" in
  let ocamldoc_options = [
    "-v";             (* run verbose                                     *)
    "-sort";          (* sort the output modules                         *)
    "-stars";         (* remove leading blank characters in doc comments *)
    "-warn-error";    (* treat errors as warnings                        *)
    "-html";          (* html output by default                          *)
    "-colorize-code"; (* provide syntax highlighting in the HTML         *)
    "-I";
    build_dir;        (* includes the output directory of cs3110 compile *)
    "-d";
    output_dir;       (* put output in output_dir                        *)
  ] in
  let mlis = try get_files_with_extension "mli" src_dir with _ -> [] in
  check_code (run_process "ocamldoc" (ocamldoc_options @ mlis))

let doc_command =
  Command.basic
    ~summary:"Generates the ocamldoc documentation for a given directory."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "The doc command is a wrapper for the ocamldoc tool. It searches the";
      "[src_dir] directory for .mli files and then runs to ocamldoc tool. The";
      "generated html is placed in the [output_dir] directory. The default";
      "[src_dir] is the current working directory."
    ])
    Command.Spec.(
      empty
      +> anon ("output_dir" %: file)
      +> anon (maybe ("src_dir" %: file)))
    doc

let run_doc () =
  Command.run ~version:"2.0" ~build_info:"Core" doc_command
