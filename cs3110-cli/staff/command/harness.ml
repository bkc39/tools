open Build
open Constants
open Core.Std
open FilepathUtil
open IOUtil
open Postscript
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

let initialize_spreadsheet test_suite () =
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
  Unix.close_process_out postscript_channel

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

(* (\** [harness tests targets] run each set of unit tests under [tests] *)
(*  * against [targets] *\) *)
(* let harness (submissions : string) (tests : string list) : unit = *)
(*   let _ = *)
(*     assert_file_exists test_dir; *)
(*     ensure_dir output_dir *)
(*   in *)
(*   let cwd = Sys.getcwd () in *)
(*   let directories = strip_trailing_slash_all directories in *)
(*   let student_part_score = ref 0 in *)
(*   (\* [test_suite] is a list of files containing tests. *\) *)
(*   let test_suite = Array.fold_right (fun f acc -> *)
(*     ((strip_suffix f)) :: acc) (Sys.readdir test_dir) [] in *)
(*   (\* Ensure rubric *\) *)
(*   let _ = *)
(*     if Sys.file_exists rubric_file *)
(*     then assert_valid_rubric () *)
(*     else create_rubric test_suite directories *)
(*   in *)
(*   let rubric = dict_of_rubric_file rubric_file in *)
(*   (\* For each implementation to test, copy in the tests, build, and run. *\) *)
(*   let _ = List.iter (fun dir -> *)
(*     (\* Prepare for testing *\) *)
(*     let netid = tag_of_path dir in *)
(*     let txt_fname = Format.sprintf "./%s/%s.md" output_dir netid in *)
(*     let txt_chn = open_out txt_fname in *)
(*     (\* Print netid to CMS, change to student dir *\) *)
(*     let _ = *)
(*       output_string cms_chn netid; *)
(*       Sys.chdir dir *)
(*     in *)
(*     (\* Print titles *\) *)
(*     let _ = *)
(*       Format.printf "\n## Running tests for '%s' ##\n%!" netid; *)
(*       output_string txt_chn (Format.sprintf "## Automated test results for %s ##\n" netid) *)
(*     in *)
(*     (\* Build and run *\) *)
(*     let _ = List.iter (fun test_name -> *)
(*       (\* Reset part score on CMS *\) *)
(*       let _ = student_part_score := 0 in *)
(*       (\* Prepare postscript document *\) *)
(*       let ps_chn = *)
(*         let fname = Format.sprintf "%s/%s/%s-%s.ps" cwd output_dir netid test_name in *)
(*         let title = Format.sprintf "%s\t\t%s.ml" netid test_name in *)
(*         ps_open_channel fname title *)
(*       in *)
(*       (\* copy source file, print header in ps stream *\) *)
(*       let _ = *)
(*         ps_set_font ps_chn ps_header_font; *)
(*         (\* Copy source to postscript. Obtaining source is a hack, but it's not fatal if it fails *\) *)
(*         let _ = *)
(*           let src_fname = Format.sprintf "%s.ml" (fst (rsplit test_name '_')) in *)
(*           if not (Sys.file_exists src_fname) then *)
(*             output_string ps_chn "SOURCE NOT FOUND\n" *)
(*           else begin *)
(*             output_string ps_chn (Format.sprintf "Source code for file '%s':\n" src_fname); *)
(*             ps_set_font ps_chn ps_code_font; *)
(*             List.iter (fun line -> *)
(*               output_string ps_chn line; output_string ps_chn "\n" *)
(*             ) (read_lines (open_in src_fname)) *)
(*           end *)
(*         in *)
(*         flush ps_chn; *)
(*         output_string txt_chn (Format.sprintf "### %s ###\n" test_name) *)
(*       in *)
(*       let _ = Sys.command (Format.sprintf "cp %s/%s.ml ." test_dir test_name) in *)
(*       let exit_code = build test_name in *)
(*       let output_by_line = *)
(*         if exit_code <> 0 then *)
(*           ["NO COMPILE"] *)
(*         else begin *)
(*           (\* Run tests, organize output *\) *)
(*           let _ = test_logging_errors test_name in *)
(*           let score, lines = harness_collect_output rubric in *)
(*           let _ = student_part_score := score in *)
(*           lines *)
(*         end *)
(*       in *)
(*       (\* Postscript title *\) *)
(*       let _ = *)
(*         ps_set_font ps_chn ps_header_font; *)
(*         output_string ps_chn "\nTest Results:\n"; *)
(*         ps_set_font ps_chn ps_normal_font *)
(*       in *)
(*       (\* Print results for each test case *\) *)
(*       let _ = List.iter (fun msg -> *)
(*         print_endline msg; *)
(*         output_string txt_chn "    "; output_string txt_chn msg; output_string txt_chn "\n"; *)
(*         output_string ps_chn msg; output_string ps_chn "\n"; *)
(*       ) output_by_line; print_endline ""; output_string ps_chn "\n" *)
(*       in *)
(*       (\* Print aggregate results to CMS *\) *)
(*       let _ = output_string cms_chn (Format.sprintf ",%d" (!student_part_score)) in *)
(*       (\* Flush and close postscript *\) *)
(*       let _ = *)
(*         flush ps_chn; *)
(*         flush txt_chn; *)
(*         Unix.close_process_out ps_chn *)
(*       in *)
(*       (\* Remove generated files *\) *)
(*       let _ = *)
(*         (\* 2014-01-19: Removing tests so they don't screw with reverse harness *\) *)
(*         ignore(Sys.command (Format.sprintf "rm %s.ml" test_name)); *)
(*         if Sys.file_exists test_output then *)
(*           ignore(Sys.command ("rm " ^ test_output)); *)
(*         if Sys.file_exists fail_output then *)
(*           ignore(Sys.command ("rm " ^ fail_output)); *)
(*         () *)
(*       in *)
(*       () *)
(*     ) test_suite in *)
(*     (\* Finished with [dir]. Clean up, print total points to CMS, move out. *\) *)
(*     let _ = *)
(*       close_out txt_chn; *)
(*       Sys.chdir cwd *)
(*     in *)
(*     (\* Write comments to the CMS spreadsheet *\) *)
(*     let _ = *)
(*       (\* Replace double quotes with single quotes *\) *)
(*       let comments = *)
(*         String.map (fun c -> if c = '"' then '\'' else c *)
(*         ) (String.concat " \n " (read_lines (open_in txt_fname))) *)
(*       in *)
(*       output_string cms_chn (Format.sprintf ",\"%s\"\n" comments) *)
(*     in *)
(*     () *)
(*   ) directories in *)
(*   let _ = close_out cms_chn in *)
(*   () *)

(* let harness_command () = failwith "TODO" *)

(* let run_harness_command () = failwith "TODO" *)
