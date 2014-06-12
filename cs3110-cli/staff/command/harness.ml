open Build
open Constants
open Core.Std
open FilepathUtil
open IOUtil
open Postscript
open Rubric
open Test

(** [harness_collect_output rubric] reads the generated files for test
  output and test failures and organizes the results.  Points are
  awarded based on whether the student passed.  Values are taken from
  the [rubric] hashtable *)
let harness_collect_output rubric : int * string list =
  let _ = assert_file_exists test_output in
  let _ = assert_file_exists fail_output in
  (* Collect test names *)
  let test_names =
    List.fold_left (read_lines (open_in test_output))
                   ~init:[]
                   ~f:(fun acc line -> let name = snd (rsplit line ':') in
                                     name :: acc) in
  (* Organize error messages *)
  let errors_by_name = Hashtbl.create ~hashable:Hashtbl.Poly.hashable () in
  let () = List.iter
             ~f:(fun line -> let name = test_name_of_line line in
                           Hashtbl.add_exn errors_by_name ~key:name ~data:line)
             (read_lines (open_in fail_output)) in
  (* Generate final list of output. Test name + pass/fail message *)
  List.fold_left
    test_names
    ~init:(0,[])
    ~f:(fun (pts, strs) name ->
        let pts', msg =
          if Hashtbl.mem errors_by_name name then
            (* a fail-er, no need to collect points *)
            match Hashtbl.find errors_by_name name with
            | Some err -> (pts, Format.sprintf "FAIL -- %s : %s" name err)
            | None     -> begin
              let err_msg =
                Printf.sprintf
                  "Error: %s\nwas not found. Check the test names again."
                  name in
              raise (Invalid_rubric err_msg)
            end
          else
            (* a passer, get column name (test file name) and point value *)
            match Hashtbl.find rubric name with
            | Some point_value -> begin
              pts + point_value, Format.sprintf "PASS -- %s" name
            end
            | None             -> begin
              let err_msg = Format.sprintf "Error: missing test '%s' in rubric"
                                           name in
              raise (Invalid_rubric err_msg)
            end in
        (pts', msg :: strs))

(** Gets all of the test files in the given directory. *)
let test_files_from_directory test_directory =
  match Sys.is_directory test_directory with
  | `Yes -> begin
    Array.fold_right (Sys.readdir test_directory)
                     ~init:[]
                     ~f:(fun file acc -> strip_suffix file :: acc)
  end
  | `No
  | `Unknown -> begin
    let err_msg = Printf.sprintf "The test suite directory '%s' is invalid\n"
                                 test_directory in
    failwith err_msg
  end

let initialize_spreadsheet test_suite =
  match Sys.file_exists cms_fname with
  | `Yes -> open_out_gen [Open_creat; Open_text; Open_append] 0o777 cms_fname
  | `No
  | `Unknown -> begin
    let cms_chn = open_out cms_fname in
    output_string cms_chn "NetID";
    List.iter
      ~f:(fun name ->
          output_string cms_chn (Format.sprintf ",%s" (String.capitalize name)))
      test_suite;
    output_string cms_chn ",Add Comments\n";
    cms_chn
  end

(** [print titles] prints a message to [text_channel] based off of the
test [netid] *)
let print_titles text_channel netid =
  Format.printf "\n## Running tests for: %s ##\n%!" netid;
  output_string text_channel
                (Format.sprintf "## Automated test results for %s ##\n" netid)

(** Returns an [in_channel] with a handle on a postscript file to
record the test results of [net_id] on the test [test_name]. *)
let prepare_postscript_channel netid test_name =
  let fname =
    Format.sprintf "%s/%s/%s-%s.ps" (Sys.getcwd ()) output_dir netid test_name in
  let title = Format.sprintf "%s\t\t%s.ml" netid test_name in
  ps_open_channel fname title

(** Copies the source in [src_fname] and writes in to [postscript_channel]. *)
let copy_source_to_postscript src_fname postscript_channel =
  match Sys.file_exists src_fname with
  | `Yes -> begin
    output_string postscript_channel
                  (Format.sprintf "Source code for file '%s':\n" src_fname);
    ps_set_font postscript_channel ps_code_font;
    List.iter ~f:(fun line -> output_string postscript_channel (line^"\n"))
              (read_lines (open_in src_fname));
    flush postscript_channel
  end
  | `No
  | `Unknown -> begin
    output_string postscript_channel "SOURCE NOT FOUND\n";
    flush postscript_channel
  end

let build_and_run test_dir test_name rubric =
  let _ = Sys.command (Format.sprintf "cp %s/%s.ml ." test_dir test_name)in
  let score_and_output = ref (0,[]) in
  let () =
    build ~with_exit_code:(fun exit_code ->
                           if exit_code <> 0 then
                             score_and_output := 0, ["NO COMPILE"]
                           else begin
                             let _ = test_logging_errors test_name in
                             score_and_output := harness_collect_output rubric
                           end)
          true
          test_name
          () in
  !score_and_output

let print_results (score,results) cms_channel postscript_channel text_channel =
  (** Format the title *)
  ps_set_font postscript_channel ps_header_font;
  output_string postscript_channel "\nTest Results:\n";
  ps_set_font postscript_channel ps_normal_font;
  (** Print the results for each case. *)
  List.iter ~f:(fun msg ->
                print_endline msg;
                output_string text_channel "    ";
                output_string text_channel msg;
                output_string text_channel "\n";
                output_string postscript_channel msg;
                output_string postscript_channel "\n")
            results;
  (* Print aggregate results to CMS *)
  output_string cms_channel (Format.sprintf ",%d" score);
  (* Flush and close postscript *)
  flush postscript_channel;
  flush text_channel;
  ignore(Unix.close_process_out postscript_channel)

let remove_generated_files test_name =
  let rm_test_output () = ignore (Sys.command ("rm " ^ test_output)) in
  let rm_fail_output () = ignore (Sys.command ("rm " ^ fail_output)) in
  (* 2014-01-19: Removing tests so they don't screw with reverse harness *)
  ignore(Sys.command (Format.sprintf "rm %s.ml" test_name));
  match Sys.file_exists test_output, Sys.file_exists fail_output with
  | `Yes,`Yes         -> rm_test_output (); rm_fail_output ()
  | `Yes,`No
  | `Yes,`Unknown     -> rm_test_output ()
  | `No,`Yes
  | `Unknown,`Yes     -> rm_fail_output ()
  | `No,`No
  | `No,`Unknown
  | `Unknown,`No
  | `Unknown,`Unknown -> ()

let run_suite netid test_dir rubric cms_channel text_channel =
  List.iter ~f:(fun test_name ->
    let postscript_channel = prepare_postscript_channel netid test_name in
    let () = copy_source_to_postscript
               (Format.sprintf "%s.ml" (fst (rsplit test_name '_')))
               postscript_channel in
    let score_and_output = build_and_run test_dir test_name rubric in
    let () = print_results score_and_output
                           cms_channel
                           postscript_channel
                           text_channel in
    remove_generated_files test_name)

let write_comments_to_cms cms_channel comments_file =
  let comments =
    String.map (String.concat ~sep:" \n " (read_lines (open_in comments_file)))
               ~f:(fun c -> if c = '"' then '\'' else c) in
  output_string cms_channel (Format.sprintf ",\"%s\"\n" comments)

let harness target_dir test_dir () =
  let all_targets = get_subdirectories target_dir in
  let () = assert_file_exists test_dir; ensure_dir output_dir in
  let cwd = Sys.getcwd () in
  let targets = strip_trailing_slash_all all_targets in
  let test_suite = test_files_from_directory test_dir in
  (* Ensure rubric *)
  let () = match Sys.file_exists rubric_file with
    | `Yes     -> assert_valid_rubric ()
    | `No
    | `Unknown -> create_rubric test_suite targets in
  let rubric = dict_of_rubric_file rubric_file in
  let cms_chn = initialize_spreadsheet test_suite in
  (* For each implementation to test, copy in the tests, build, and run. *)
  let () = List.iter ~f:(fun implementation_directory ->
    (* Prepare for testing *)
    let netid = tag_of_path implementation_directory in
    let txt_fname = Format.sprintf "./%s/%s.ml" output_dir netid in
    let txt_chn = open_out txt_fname in
    (* Print netid to CMS, change to student dir *)
    let () = output_string cms_chn netid;
             Sys.chdir implementation_directory;
             (* Print titles *)
             print_titles txt_chn netid in
    (* Run the test suite *)
    run_suite netid test_dir rubric cms_chn txt_chn test_suite;
    (* Done with [implementation_directory]. Clean up, print total
    points to CMS, dip out. *)
    Out_channel.close txt_chn;
    Sys.chdir cwd;
    write_comments_to_cms cms_chn txt_fname) targets in
  Out_channel.close cms_chn

let harness_command =
  Command.basic
    ~summary:"Runs the CS3110 test harness."
    ~readme:(fun () -> String.concat ~sep:"\n" [
       "The harness command runs the test harness against all entries in a";
       "target directory. [cs3110-staff harness <targets>] runs the tests";
       "in the directory 'ests' against the submissions in '<targets>'."
    ])
    Command.Spec.(
    empty
    +> anon ("targets" %: file)
    +> anon (maybe_with_default "./tests" ("test-dir" %: file)))
    harness

let run_diff () =
  Command.run ~version:"2.0" ~build_info:"Core" harness_command
