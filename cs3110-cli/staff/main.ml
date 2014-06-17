open Constants
open Postscript
open IOUtil
open FilepathUtil
open ProcessUtil

(**** cs3110 commands *********************************************************)

(** [clean ()] remove all files generated during compilation *)
let clean () : unit =
  check_code (Sys.command "ocamlbuild -clean")

(** [build] compile [m] into a bytecode executable. 
 * Relies on ocamlbuild. TODO quiet version? *) 
let build (main_module : string) : int =
  assert_file_exists (main_module ^ ".ml");
  let target = Format.sprintf "%s.d.byte" main_module in
  let _ = Format.printf "Compiling '%s.ml'\n%!" main_module in
  let dependencies = 
    if Sys.file_exists depend_file
    then ["-Is"; csv_of_file depend_file]
    else []
  in
  let libraries = 
    if Sys.file_exists lib_file
    then ["-libs"; "assertions," ^ csv_of_file lib_file]
    else ["-libs"; "assertions"]
  in
  let all_opam_packages = std_opam_packages @ 
    if Sys.file_exists opam_packages_file
    then (read_lines (open_in opam_packages_file))
    else []
  in
  let opam_packages_str = 
      (String.concat ", " 
      (List.map (fun p -> Format.sprintf "package(%s)" p) all_opam_packages))
  in
  run_process "ocamlbuild" (dependencies @ libraries @ [
    "-cflag"; "-warn-error"; "-cflag"; "+a"; (* treat the default warnings as errors *)
    "-use-ocamlfind"; "-no-links"; 
    "-tag-line"; "<*.ml{,i}> : syntax(camlp4o), " ^ opam_packages_str;
    "-tag-line"; "<*.d.byte> : " ^ opam_packages_str;
    "-tag-line"; "<*.native> : " ^ opam_packages_str;
    target
  ])

let doc ?(src_dir=Sys.getcwd ()) (output_dir : string) : int =
  Printf.printf "Generating documentation for directory: %s\n" src_dir;
  let build_dir =
    try
      if Sys.is_directory "_build" then "_build"
      else begin
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
  run_process "ocamldoc" (ocamldoc_options @ mlis)

(** [diff ()] if any files under the [nocompile_dir] match a target,
 * perform a diff on the pre-and post submission. Record results in a csv file. *)
let diff (directories : string list) : unit =
  let _ = assert_file_exists nocompile_dir in
  (* Space to store outputs *)
  let diff_tmp = ".diff" in
  (* maps netid -> dir, to make it easy to get new input from the 
  * name of a previous no-compile *)
  let netid_map = Hashtbl.create 27 in 
  let _ = List.iter (fun dir -> 
    Hashtbl.add netid_map (tag_of_path dir) dir)
    (strip_trailing_slash_all directories) in
  let num_diffed = ref 0 in
  let results_chn = open_out diff_results in
  (* For each file in the nocompile folder, check if it has a match in [directories] *)
  Array.iter (fun netid -> 
    if Hashtbl.mem netid_map netid then
      let dir = Hashtbl.find netid_map netid in
      (* Found a match. diff this submission *)
      let user_input = ref 2 in
      let result = Array.fold_left (fun acc fname ->
        if acc = 0 then 
          (* Already rejected student's fix for previous file. 
           * They suffer the penalty. *)
          0
        else begin
          (* Run a diff, ask user for judgement. 
           * Automatically handles missing files and 'no difference' resubmits *)
          let new_file = Format.sprintf "%s/%s" dir fname in
          let cmd = Format.sprintf "diff %s %s/%s/%s" new_file nocompile_dir netid fname in
          let _ = Format.printf "\n### Executing '%s' ###\n%!" cmd in
          if not (Sys.file_exists new_file) then
            (* Resubmission does not contain a file. 
             * Return 1 because they do not have a late submission *)
            let _ = Format.printf "File '%s' does not exist!\n%!" new_file in
            1
          else 
            let _ = Sys.command (Format.sprintf "%s > %s" cmd diff_tmp) in
            begin match (read_lines (open_in diff_tmp)) with
              | [] -> 
                (* No differences. Print and return 1. It's 1 
                 * because they did not submit late *)
                let _ = Format.printf "diff of '%s' finished with no differences\n%!" in
                1
              | h::t as lines ->
                (* Non-empty diff! Display it and wait for user response *)
                (* Set up to collect input *)
                let _ = 
                  List.iter print_endline lines;
                  user_input := 2;
                  print_string "** "
                in
                (* Collect input *)
                while (!user_input <> 0 && !user_input <> 1) do (
                  print_endline "Please enter 0 or 1 as judgement. 1 is good.";
                  try user_input := int_of_string (read_line ()) with
                    | Failure "int_of_string" -> ()
                ) done;
                (* Shut down *)
                let _ = 
                  print_endline "Ok! ";
                  incr num_diffed
                in
                !user_input
            end
        end 
      (* btw, initial accumulator for the fold is 1 because of the guard on 0 
       * If [acc = 0], we stop doing diffs for the student. *)
      ) 1 (Sys.readdir (Format.sprintf "%s/%s" nocompile_dir netid)) in
      (* Save the results to the .csv *)
      output_string results_chn (Format.sprintf "%s,%d\n" netid result)
  ) (Sys.readdir "./_nocompile");
  (* Close up *)
  let _ = 
    ignore(Sys.command (Format.sprintf "rm -f %s" diff_tmp));
    close_out results_chn
  in
  Format.printf "Finished diffing %d files. See '%s' for results\n%!" (!num_diffed) diff_results

(** [email ()] send the email messages stored in the _email directory.  *
 * Assumes that every folder name under _email is a valid Cornell netid *)
let email () : unit = 
  let _ = 
    assert_file_exists email_dir;
    assert_file_exists email_admins;
  in
  let messages = Sys.readdir email_dir in
  let num_sent = ref 0 in
  (* Use the [mail] command to package off the email message *)
  Array.iter (fun (msg_file : string) ->
    let recipient = (strip_suffix msg_file) ^ "@cornell.edu" in
    let bcc = Format.sprintf "-b '%s'" (String.concat "' -b '" (read_lines (open_in email_admins))) in
    let cmd = Format.sprintf "mutt -s '%s' %s '%s' < %s/%s" email_subject bcc recipient email_dir msg_file in
    let _ = Format.printf "### Executing '%s'\n%!" cmd in
    let exit_code = Sys.command cmd in
    if exit_code <> 0 then 
      print_endline ("Failed to send message to: " ^ recipient)
    else
      incr num_sent
  ) messages;
  Format.printf "Finished sending %d messages.\n%!" !num_sent

(* The general form of the test command *)
let test_parameterized (main_module : string) (output : string) : int =
  let exec = Format.sprintf "_build/%s.d.byte" main_module in
  let _ = assert_file_exists exec in
  let cmd = Format.sprintf "%s inline-test-runner dummy -log %s" exec output in
  Sys.command cmd

(* Redirects stderr to stdout's destination, changes stdout's 
 * destination to null, saves the error messages to a file *)
let test_logging_errors (main_module :string) : int =
  (* 2014-01-11: Steals only the first line of output for each error *)
  let dest = "2>& 1>/dev/null | grep '^File' > " ^ fail_output in
  test_parameterized main_module dest

let test_quiet (main_module : string) =
  test_parameterized main_module "> /dev/null"

(** [test file] executes the unit tests within [file].
 * Inner workings documented here: [https://github.com/janestreet/pa_ounit]
 * Standard output is ignored; exceptions still print.
 * This is the default and preferred way of running test. *)
let test (main_module : string) : int =
  test_quiet main_module

(**** rubric creation *********************************************************)

(* Return a function that ensures one line of rubric is valid.
 * It is a generator so that it can track line numbers. *)
let assert_valid_line () : string -> unit = 
  let lineno = ref 0 in 
  (fun (raw_line : string) ->
    let ln = String.trim raw_line in
    let _ = incr lineno in
    (* Valid lines are either comments, starting with an octothorp, or empty *)
    let is_comment = (String.length ln = 0) || (ln.[0] = '#') in
    (* Else they are 'key:val' pairs, with exactly one colon and non-whitespace strings on either end *)
    let l_raw, r_raw = lsplit ln ':' in
    let l, r = (String.trim l_raw), (String.trim r_raw) in
    let has_two_colons = (String.contains r ':') in
    let empty_field = (String.length l = 0) || (String.length r = 0) in
    let right_not_int = try ignore(int_of_string r); false with Failure "int_of_string" -> true in
    (* Else they are not valid lines *)
    if (not is_comment) && (has_two_colons || empty_field || right_not_int) then
      raise (Invalid_rubric (Format.sprintf "Rubric '%s' is not valid. Error on line %d" rubric_file (!lineno)) )
  )

(* Ensure the rubric file is correctly formatted *)
let assert_valid_rubric () : unit =
  List.iter (assert_valid_line ()) (read_lines (open_in rubric_file))

(** [create_rubric suite targets] Use the test files ([suite]) and a batch of 
 * solutions ([targets]) to build a properly formatted rubric *)
let create_rubric (test_suite : string list) (dirs : string list) : unit = 
  let cwd = Sys.getcwd () in
  let _ = Format.printf "Let's make a rubric!\n\
*********************************************************************\n\
**** DISCLAIMER: cs3110 supports a very limited subset of YAML   ****\n\
**** Lines must be comments (beginning with an octothorp, #) or  ****\n\
**** 'test_name:int_value' pairs. That's right, a proper line of ****\n\
**** data has a string then a colon then an integer.             ****\n\
**** Whitespace is ignored.                                      ****\n\
*********************************************************************\n\n"
  in
  let _ = Format.printf "First thing: searching for a directory that compiles.\n" in
  let test_dir = absolute_path tests_dir in
  (* Iterate over students. Build each of their files. Find the first one that compiles for everything. *)
  let good_student = 
    let dir_opt = List.fold_left (fun dir_opt new_dir ->
      match dir_opt with 
        | Some dir -> dir_opt (* already found a good student, skip rest of directories *)
        | None -> (* Go to student's folder, copy in the tests, compile each, check output *)
          let _ = Sys.chdir new_dir in
          (* Copy the tests over, compile  each *) 
          let all_compile = List.fold_left (fun acc test_name ->
            acc && (let _ = Sys.command (Format.sprintf "cp %s/%s.ml ." test_dir test_name) in
                    let exit_code = build test_name in exit_code = 0)
          ) true test_suite in
          let _ = Sys.chdir cwd in
          if all_compile then Some new_dir else None
    ) None dirs in
    (* See if we found a submission that compiled everything or not *)
    match dir_opt with 
      | Some d -> 
        let _ = Format.printf "Fantastic. Directory '%s' compiles all tests. Will use it to generate test names.\n" d in
        d
      | None -> 
        raise (Invalid_rubric "Uh-oh! Could not generate a rubric.\n(Then again, you don't need one. Either everybody failed or the tests are bogus.)\n")
  in
  (* Got a good directory to build test executable in. Go ahead & collect names *)
  let _ = Format.printf "\n\
Okay then, let's get started. I will echo the name of each test.\n\
Your job is to input an INTEGER point value after each name.\n\
Output will be saved into '%s'.\n\
Test names are inferred from the '%s' folder, so edit that if something seems out of place.\n\
ReadysetGO!       \n"  rubric_file tests_dir in
  let _ = Sys.chdir good_student in
  (* Collect list of unit tests for each test file. 
   * Organize in list of (string * string list) *)
  let test_names = List.rev (List.fold_left (fun all_names test_file_full ->
    let test_file = strip_suffix test_file_full in
    (* Run the test to print names *)
    let cmd = Format.sprintf "./_build/%s.d.byte inline-test-runner dummy -list-test-names > %s" test_file test_output in
    let _ = Sys.command cmd in
    (* Generated file contains one line per unit test *)
    let names = List.rev (List.fold_left (fun acc line ->
      let name = test_name_of_line line in
      name :: acc
    ) [] (read_lines (open_in test_output))) in
    (test_file , names) :: all_names
  ) [] test_suite) in
  let _ = Sys.chdir cwd in
  let chn = open_out rubric_file in
  let response = ref (-1) in
  (* Query for point totals per test *)
  let _ = List.iter (fun (file_name, unittest_names) ->
    let _ = Format.printf "\n## Collecting point values for file '%s' ##\n" file_name in
    let _ = output_string chn (Format.sprintf "# %s\n" file_name) in
    (* For each unit test, ask the user for a point value & save it *)
    List.iter (fun name ->
      while ((!response < 0) || (!response > 100)) do (
        let _ = 
          Format.printf "Enter an integer point value for '%s' (between 0 and 100): \n" name;
          try response := read_int () with 
            | Failure "int_of_string" ->
              print_endline "Sorry, try again"; response := (-1)
        in ()
      ) done; 
      (* Save the point value to the rubric *)
      let _ = 
        output_string chn (Format.sprintf "%s : %d\n" name (!response));
        Format.printf "Ok! ";
        response := (-1)
      in flush chn
    ) unittest_names
  ) test_names in
  let _ = flush chn; close_out chn in
  Format.printf "Successfully recorded rubric in file '%s'\n" rubric_file

(** [dict_of_rubric f] read in the yaml-eqsue file [f]. Create a 
 * dictionary of (unit_test_name -> (file_name * point_value)) *)
let dict_of_rubric_file (f : string) = 
  (* unit_test_name -> (test_file_name * int) dict) *)
  let rubric = Hashtbl.create 17 in
  (* State while parsing file. 
   * Track line number and filename *)
  let lineno = ref 0 in
  let _ = List.iter (fun raw_line ->
    let _ = incr lineno in
    let line = String.trim raw_line in
    if (String.length line) > 0 && line.[0] <> '#' then
      (* Line is hopefully a test. Pretend that it is *)
      let raw_name, raw_points = lsplit line ':' in
      let name = String.trim raw_name in
      let points = 
        try int_of_string raw_points with 
          | Failure "int_of_string" ->
            raise (Invalid_rubric (Format.sprintf "Rubric '%s' is not valid. Error on line %d\n" rubric_file (!lineno)))
      in
      Hashtbl.add rubric name (points)
  ) (read_lines (open_in f)) 
  in rubric

let reverse_create_rubric (fname : string) (suite : string list): unit =
  let _ = Format.printf "ATTENTION: need to create a rubric for the reverse tests.\n\
Let me ask you a few questions about each dummy implementation.\n\
First, whether the implementation is supposed to pass all tests\n
and second how many points the outcome is worth:\n\n%!" in
  let chn = open_out fname in
  let int_response = ref (-1) in
  let str_response = ref " " in (* Need at least one character for the guard on the while loop *)
  let _ = List.iter (fun name ->
    (* Loop until user inputs 'Y' or 'N'. Ignores case and trailing characters *)
    while (((!str_response).[0] <> 'Y') && ((!str_response).[0] <> 'N')) do (
      let _ = Format.printf "Should test implementation '%s' pass all tests? (Y/N)\n%!" name in
      str_response := (String.uppercase (read_line ())) ^ " "
    ) done;
    let _ = print_string "Ok! " in
    (* Loop until the user inputs a positive integer, even if it's over 100 *)
    while ((!int_response < 0) || (!int_response > 100)) do (
      let _ = Format.printf "How many points is '%s' worth? (Give an integer between 0 and 100\n%!" name in
      try int_response := read_int () with
        | Failure "int_of_string" -> (Format.printf "Sorry, try again\n"; int_response := (-1))
    ) done;
      (* Save results to rubric *)
    print_string "Ok! ";
    output_string chn (Format.sprintf "%s : %s : %d\n" name (!str_response) (!int_response));
      (* Reset and such *)
    int_response := (-1);
    str_response := " ";
    ()
  ) suite in
  let _ = close_out chn in
  Format.printf "Finished creating reversed rubric\n%!"

(* Read a dictionary from a file of "test : should_pass : points" 
 * sample line would be: "mytest : Y:99" *)
let reverse_dict (rubric_file : string) = 
  let d = Hashtbl.create 7 in
  let () = List.iter (fun line ->
    if not (line.[0] = '#') then
      (* Not a comment. Pls process *)
      let raw_name, rest = lsplit line ':' in
      let raw_should_pass, raw_points = lsplit rest ':' in
      let name = String.trim raw_name in
      let should_pass = 
        match (String.trim raw_should_pass).[0] with
          | 'Y' -> true
          | 'N' -> false
          | _ -> raise (Invalid_rubric (Format.sprintf "Cannot tell whether test '%s' should pass or fail in '%s'\n" name rubric_file))
      in
      let points = 
        try int_of_string raw_points with
          | Failure "int_of_string" -> raise (Invalid_rubric "Malformed reverse rubric")
      in
      Hashtbl.add d name (should_pass, points)
  ) (read_lines (open_in rubric_file)) in
  d

(**** back to cs3110 commands *************************************************)

(** [harness_collect_output rubric] reads the generated files for
 * test output and test failures and organizes the results.
 * Points are awarded based on whether the student passed.
 * Values are taken from the [rubric] hashtable *)
let harness_collect_output (rubric) : int * string list =
  let _ = assert_file_exists test_output in
  let _ = assert_file_exists fail_output in
  (* Collect test names *)
  let test_names = List.fold_left (fun acc line -> 
    let name = snd (rsplit line ':') in
    name :: acc) [] (read_lines (open_in test_output)) 
  in
  (* Organize error messages *)
  let errors_by_name = Hashtbl.create 27 in
  let _ = List.iter (fun line -> 
    let name = test_name_of_line line in
    Hashtbl.add errors_by_name name line
  ) (read_lines (open_in fail_output)) in
  (* Generate final list of output. Test name + pass/fail message *)
  List.fold_left (fun (pts, strs) name ->
    let pts', msg = 
      if Hashtbl.mem errors_by_name name then
        (* a fail-er, no need to collect points *)
        let err = Hashtbl.find errors_by_name name in
        (pts, Format.sprintf "FAIL -- %s : %s" name err)
      else 
        (* a passer, get column name (test file name) and point value *)
        let new_points = 
          if not (Hashtbl.mem rubric name)
          then raise (Invalid_rubric (Format.sprintf "Error: missing test '%s' in rubric\n" name))
          else Hashtbl.find rubric name
        in
        (pts + new_points, Format.sprintf "PASS -- %s" name)
    in (pts', msg :: strs)) (0, []) test_names

(** [harness tests targets] run each set of unit tests under [tests] 
 * against [targets] *)
let harness (test_dir : string) (directories : string list) : unit =
  let _ = 
    assert_file_exists test_dir;
    ensure_dir output_dir
  in
  let cwd = Sys.getcwd () in
  let directories = strip_trailing_slash_all directories in
  let student_part_score = ref 0 in
  (* [test_suite] is a list of files containing tests. *)
  let test_suite = Array.fold_right (fun f acc -> 
    ((strip_suffix f)) :: acc) (Sys.readdir test_dir) [] in
  (* Ensure rubric *)
  let _ =
    if Sys.file_exists rubric_file
    then assert_valid_rubric ()
    else create_rubric test_suite directories
  in
  let rubric = dict_of_rubric_file rubric_file in
  (* Initialize spreadsheet for CMS *)
  let cms_chn = 
    if Sys.file_exists cms_fname then
      (* Open existing file *)
      open_out_gen [Open_creat; Open_text; Open_append] 0o777 cms_fname
    else 
      let cms_chn = open_out cms_fname in
      output_string cms_chn "NetID";
      List.iter (fun name ->
        output_string cms_chn (Format.sprintf ",%s" (String.capitalize name))
      ) test_suite;
      output_string cms_chn ",Add Comments\n";
      cms_chn
  in
  (* For each implementation to test, copy in the tests, build, and run. *)
  let _ = List.iter (fun dir -> 
    (* Prepare for testing *)
    let netid = tag_of_path dir in
    let txt_fname = Format.sprintf "./%s/%s.md" output_dir netid in
    let txt_chn = open_out txt_fname in
    (* Print netid to CMS, change to student dir *)
    let _ = 
      output_string cms_chn netid;
      Sys.chdir dir 
    in
    (* Print titles *)
    let _ = 
      Format.printf "\n## Running tests for '%s' ##\n%!" netid;
      output_string txt_chn (Format.sprintf "## Automated test results for %s ##\n" netid)
    in
    (* Build and run *)
    let _ = List.iter (fun test_name ->
      (* Reset part score on CMS *)
      let _ = student_part_score := 0 in
      (* Prepare postscript document *)
      let ps_chn =
        let fname = Format.sprintf "%s/%s/%s-%s.ps" cwd output_dir netid test_name in
        let title = Format.sprintf "%s\t\t%s.ml" netid test_name in
        ps_open_channel fname title
      in
      (* copy source file, print header in ps stream *)
      let _ =
        ps_set_font ps_chn ps_header_font;
        (* Copy source to postscript. Obtaining source is a hack, but it's not fatal if it fails *)
        let _ = 
          let src_fname = Format.sprintf "%s.ml" (fst (rsplit test_name '_')) in
          if not (Sys.file_exists src_fname) then
            output_string ps_chn "SOURCE NOT FOUND\n"
          else begin
            output_string ps_chn (Format.sprintf "Source code for file '%s':\n" src_fname);
            ps_set_font ps_chn ps_code_font;
            List.iter (fun line ->
              output_string ps_chn line; output_string ps_chn "\n"
            ) (read_lines (open_in src_fname))
          end
        in
        flush ps_chn;
        output_string txt_chn (Format.sprintf "### %s ###\n" test_name)
      in
      let _ = Sys.command (Format.sprintf "cp %s/%s.ml ." test_dir test_name) in
      let exit_code = build test_name in
      let output_by_line = 
        if exit_code <> 0 then 
          ["NO COMPILE"]
        else begin
          (* Run tests, organize output *)
          let _ = test_logging_errors test_name in
          let score, lines = harness_collect_output rubric in
          let _ = student_part_score := score in
          lines
        end
      in
      (* Postscript title *)
      let _ =
        ps_set_font ps_chn ps_header_font;
        output_string ps_chn "\nTest Results:\n";
        ps_set_font ps_chn ps_normal_font
      in
      (* Print results for each test case *)
      let _ = List.iter (fun msg -> 
        print_endline msg; 
        output_string txt_chn "    "; output_string txt_chn msg; output_string txt_chn "\n";
        output_string ps_chn msg; output_string ps_chn "\n";
      ) output_by_line; print_endline ""; output_string ps_chn "\n"
      in
      (* Print aggregate results to CMS *)
      let _ = output_string cms_chn (Format.sprintf ",%d" (!student_part_score)) in
      (* Flush and close postscript *)
      let _ = 
        flush ps_chn;
        flush txt_chn;
        Unix.close_process_out ps_chn
      in
      (* Remove generated files *)
      let _ = 
        (* 2014-01-19: Removing tests so they don't screw with reverse harness *)
        ignore(Sys.command (Format.sprintf "rm %s.ml" test_name));
        if Sys.file_exists test_output then 
          ignore(Sys.command ("rm " ^ test_output));
        if Sys.file_exists fail_output then 
          ignore(Sys.command ("rm " ^ fail_output));
        ()
      in
      ()
    ) test_suite in
    (* Finished with [dir]. Clean up, print total points to CMS, move out. *)
    let _ = 
      close_out txt_chn;
      Sys.chdir cwd 
    in
    (* Write comments to the CMS spreadsheet *)
    let _ =
      (* Replace double quotes with single quotes *)
      let comments = 
        String.map (fun c -> if c = '"' then '\'' else c
        ) (String.concat " \n " (read_lines (open_in txt_fname)))
      in
      output_string cms_chn (Format.sprintf ",\"%s\"\n" comments)
    in
    ()
  ) directories in
  let _ = close_out cms_chn in
  ()

(** [run file args] run the executable generated by [cs3110 compile file] *)
let run (main_module : string) (args : string list) : int =
  let cmd = Format.sprintf "_build/%s.d.byte" main_module in
  assert_file_exists cmd;
  run_process cmd args

(** [reverse tests targets] Run a reverse harness on [targets], 
 * generating output for CMS. That is:
 * - extract test cases from each target
 * - run test cases on each of the fake students in [tests]
 * - match expected vs. actual (pass/fail)
 * This runs very similar to harness. Should share functionality. 
 * 2014-01-19: currently supports exactly one test file. 
 * 2014-01-19: This code eventually needs cleaning and abstraction.
 * Thing is, I don't trust in its future. Would rather delete it. *)
let reverse (test_name : string) (tests_dir : string) (targets : string list) =
  (* TODO add in point bonus for having at least [n] tests? *)
  (* Get names of all dummies. This is a list of folder names *)
  let suite = Array.fold_right (fun f acc -> 
    (tests_dir ^ "/" ^ (strip_suffix f)) :: acc) (Sys.readdir reverse_dir) [] in
  (* Set up reverse_rubric *)
  let rubric =
    if not (Sys.file_exists reverse_rubric) then
      reverse_create_rubric reverse_rubric suite;
    reverse_dict reverse_rubric
  in
  (* Set up CMS *)
  let cms_chn = open_out reverse_cms in
  let _ = output_string cms_chn "NetID,Tests,Add Comments\n" in
  let cwd = Sys.getcwd () in
  let total_points = ref 0 in
  (* For each target, run target's test on each dummy in suite *)
  List.iter (fun target_dir ->
    let netid = tag_of_path target_dir in
    (* Update CMS with netid *)
    let _ = output_string cms_chn netid in
    let target_abs = absolute_path target_dir in
    (* Set up md, ps *)
    let txt_fname = Format.sprintf "%s/%s-reverse.md" output_dir netid in
    let txt_chn = open_out (txt_fname) in
    let ps_chn = 
      let fname = Format.sprintf "%s/%s/%s-reverse.ps" cwd output_dir netid in
      let title = Format.sprintf "%s\t\t%s.ml" netid test_name in
      ps_open_channel fname title
    in
    (* Write headings and code to postscript *)
    let _ = 
      output_string txt_chn (Format.sprintf "## Reverse test results for %s ##\n" netid);
      output_string ps_chn (Format.sprintf "Source code for file '%s':\n" test_name);
      ps_set_font ps_chn ps_code_font;
      List.iter (fun line -> 
        output_string ps_chn line; output_string ps_chn "\n"
      ) (read_lines (open_in (Format.sprintf "%s/%s.ml" target_dir test_name)));
      (* ps title *)
      ps_set_font ps_chn ps_header_font;
      output_string ps_chn "\nTest Results:\n";
      ps_set_font ps_chn ps_normal_font
    in
    (* Run the tests *)
    List.iter (fun test_dir ->
      Sys.chdir test_dir;
      let passed = 
        let _ = Sys.command (Format.sprintf "cp %s/%s.ml ." target_abs test_name) in
        let exit_code = build test_name in
        if exit_code <> 0 then
          false
        else 
          let _ = test_logging_errors test_name in
          let r = begin match (read_lines (open_in fail_output)) with
            | [] -> true
            | h::t -> false
          end in
          let _ = Sys.command (Format.sprintf "rm %s.ml %s" test_name fail_output) in
          r
      in
      (* Expected results *)
      let should_pass, points_possible = Hashtbl.find rubric test_dir in
      (* Match expected vs. actual pass/fail status *)
      let points_earned, msg = match should_pass = passed with
        | true -> 
          total_points := (!total_points) + points_possible;
          points_possible, Format.sprintf "PASS -- %s\n" test_dir
        | false -> 
          0, Format.sprintf "FAIL -- %s\n" test_dir
      in
      (* Print results to md, ps *)
      let _ = 
        output_string ps_chn msg;
        output_string txt_chn msg
      in
      Sys.chdir cwd
    ) suite;
    (* Close channels *)
    let _ =
      flush ps_chn;
      flush txt_chn; 
      close_out txt_chn;
      Unix.close_process_out ps_chn
    in
    (* Print results to CMS, close ps, txt *)
    let _ = 
      let comments = 
        String.map (fun c -> if c = '"' then '\'' else c
        ) (String.concat " \\n " (read_lines (open_in txt_fname)))
      in
      output_string cms_chn (Format.sprintf ",%d,\"%s\"\n" (!total_points) comments);
    in
    (* Reset accumulator *)
    total_points := 0
  ) targets;
  flush cms_chn;
  close_out cms_chn

(** [smoke_compile_one ms d] compile each module in the list [ms] under the 
 * containing directory [d]. Generate emails and save the files for failures. *)
let smoke_compile_one (targets : string list) (dir_name : string) : unit =
  let _ = Format.printf "\n## Smoke target '%s' ##\n" (tag_of_path dir_name) in
  let failed_targets : string list ref = ref [] in
  let compile_and_record (target : string) : unit = 
    (* 2014-01-09: [build] directs compiler output to stdout/stderr.  *
     * Could redirect this to the email.                              *)
    let fname = Format.sprintf "%s.ml" target in
    if not (Sys.file_exists fname) then 
      let _ = Format.printf "'%s' not found\n%!" fname in 
      failed_targets := target :: !failed_targets
    else if build target <> 0 then
      failed_targets := target :: !failed_targets
  in
  let cwd = Sys.getcwd () in
  let _ = Sys.chdir dir_name in
  List.iter compile_and_record targets;
  let _ = Sys.chdir cwd in
  (* If there were failures, record an email message *)
  match !failed_targets with 
    | [] -> ()
    | h::t -> 
      let name = tag_of_path dir_name in 
      (* Write the email message *)
      (* 2014-01-09: Sorry, I'd love to save the email message at the *
       * top of this file, but that's not allowed for format strings. *)
      let message = Format.sprintf "Dear %s, \n\
\n\
The following files from your CMS submission were either missing or failed to compile via `cs3110 compile` \n\
* %s.ml\n\n\
Please update your submission on CMS so that `cs3110 compile <file>` succeeds for each of the above files. If the required changes were small, you will not be charged a late/slip day. If the changes were non-trivial, you will lose a slip day if you have any remaining or the late penalty. \n\
Good luck!\n\
\n\
--- Automatically generated message from the CS3110 test harness ---\n\
" name (String.concat ".ml\n* " (List.rev !failed_targets)) in
      let email_chn = open_out (Format.sprintf "./_email/%s.txt" name) in
      output_string email_chn message;
      close_out email_chn;
      (* Save each failing source file *)
      let nocompile_dir = Format.sprintf "./_nocompile/%s" name in
      ensure_dir nocompile_dir;
      let copy_file (target : string) =
        let fname = Format.sprintf "%s/%s.ml" dir_name target in
        (* Either copy the existing source code or initialize an empty file. 
         * The empty file will give an 100% diff later, whereas no file would
         * raise an error. *)
        let exit_code = 
          if not (Sys.file_exists fname) 
          then Sys.command (Format.sprintf "touch %s/%s.ml" nocompile_dir target)
          else Sys.command (Format.sprintf "cp %s %s" fname nocompile_dir)
        in
        if exit_code <> 0 
        then Format.printf "ERROR: Failed to save file %s/%s.ml\n" dir_name target
      in
      List.iter copy_file !failed_targets

(** 
 * [smoke dirs] Attempt to compile each target in each directory of [dirs]. 
 * Failures are recorded in email messages and the files are saved under the 
 * ./_nocompile folder
 *)
let smoke (directories : string list) : unit =
  let directories = strip_trailing_slash_all directories in
  (* setup *)
  let _ = 
    ensure_dir email_dir;
    ensure_dir nocompile_dir
  in
  (* Try to infer targets from test names. Assuming all tests are 
   * of form 'file_test.ml' *)
  let targets = 
    if Sys.file_exists smoke_targets
    then List.map strip_suffix (read_lines (open_in smoke_targets))
    else if Sys.file_exists tests_dir
    then Array.fold_right (fun f acc -> 
      ((fst(rsplit f '_'))) :: acc) (Sys.readdir tests_dir) [] 
    else raise (File_not_found smoke_targets)
  in
  (* Compile the targets in one directory. Accumulate an email message  *
   * outlining the failures.                                            *)
  List.iter (smoke_compile_one targets) directories

let help () =
  print_string "\
Usage: cs3110-staff COMMMAND [args]

  cs3110 clean                         Removes files created by 'cs3110 compile'
  cs3110 compile <file>                Compile file.ml.
  cs3110 diff <targets>                Compare supplied files with those stored
                                        as no compiles.
  cs3110 doc [src_dir] <output_dir>    Generates the ocamldoc documentation for
                                       the .mli files in [src_dir] and dumps
                                       output to <output_dir>. Default src_dir
                                       is the current working directory.
  cs3110 email                         Send the emails generated by the smoke
                                        test.
  cs3110 harness <targets>             Run the tests in the directory ./tests
                                        against all targets.
  cs3110 help                          Displays this message.
  cs3110 reverse <test_name> <targets> Run test named test_name.ml from each of
                                        the targets on the solutions
                                        in [reverse_dir].
  cs3110 rubric <sol_dir>              Create a rubric using the implementations
                                        in sol_dir to compile the tests.
  cs3110 run <file>                    Run the program file.ml.
  cs3110 smoke <targets>               Compile all targets.
  cs3110 test <file>                   Run the tests in file.ml.
"

(**** main function ***********************************************************)

let () = 
  let _ = 
    config_env ();
    ensure_dir cms_dir
  in
  try
    match Array.to_list Sys.argv with
    | [ _; "help" ]  -> help ()
    | [ _; "clean" ] -> clean ()
    | [ _; "compile"; target ] -> check_code (build (strip_suffix target))
    |  _ :: "diff" :: arg1 :: args -> 
      if arg1.[0] = '@'
      then diff (directories_of_list arg1)
      else diff (arg1 :: args)
    | [ _; "email" ] -> email ()
    | _ :: "harness" :: arg1 :: args -> 
      (* Make sure test dir exists *)
      let _ = assert_file_exists tests_dir in
      (* convert to absolute path *)
      let abs_dir = absolute_path tests_dir in 
      (* Run harness *)
      if arg1.[0] = '@'
      then harness abs_dir (directories_of_list arg1)
      else harness abs_dir (arg1 :: args)
    | _ :: "reverse" :: test_name :: arg1 :: args -> 
      let test_name = strip_suffix test_name in
      (* Make sure test dir exists *)
      let _ = assert_file_exists reverse_dir in
      (* convert to absolute path *)
      let abs_dir = absolute_path reverse_dir in 
      (* Run reverse harness *)
      if arg1.[0] = '@'
      then reverse test_name abs_dir (directories_of_list arg1)
      else reverse test_name abs_dir (arg1 :: args)
    | _ :: "rubric" :: solutions -> 
      let _ = assert_file_exists tests_dir in
      let test_suite = Array.fold_right (fun f acc -> 
        ((strip_suffix f)) :: acc) (Sys.readdir tests_dir) [] in
      create_rubric test_suite solutions
    | _ :: "run" :: target :: args -> 
        let target' = strip_suffix target in check_code (run target' args)
    | _ :: "smoke" :: arg1 :: args -> 
      if arg1.[0] = '@'
      then smoke (directories_of_list arg1)
      else smoke (arg1 :: args)
    | [ _; "test"; target ] -> 
        let target' = strip_suffix target in check_code (test target')
    | [ _; "doc"; src_dir; output_dir] -> begin
      check_code (doc ~src_dir:src_dir output_dir)
    end
    | [ _; "doc"; output_dir] -> begin
      check_code (doc output_dir)
    end
    | _ -> print_endline "Invalid arguments."; help ()
  with File_not_found filename ->
    Format.printf "Could not find the file %s.\n%!" filename
