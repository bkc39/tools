open Build
open Constants
open Core.Std
open FilepathUtil
open IOUtil

(* Return a function that ensures one line of rubric is valid.
 * It is a generator so that it can track line numbers. *)
let assert_valid_line () : string -> unit =
  let lineno = ref 0 in
  (fun (raw_line : string) ->
   let ln = String.strip raw_line in
   let _ = incr lineno in
   (* Valid lines are either comments, starting with an octothorp, or empty *)
   let is_comment = (String.length ln = 0) || (ln.[0] = '#') in
   (* Else they are 'key:val' pairs, with exactly one colon and non-whitespace strings on either end *)
   let l_raw, r_raw = lsplit ln ':' in
   let l, r = (String.strip l_raw), (String.strip r_raw) in
   let has_two_colons = (String.contains r ':') in
   let empty_field = (String.length l = 0) || (String.length r = 0) in
   let right_not_int = try ignore(int_of_string r); false with Failure "int_of_string" -> true in
   (* Else they are not valid lines *)
   if (not is_comment) && (has_two_colons || empty_field || right_not_int) then
     raise (Invalid_rubric (Format.sprintf "Rubric '%s' is not valid. Error on line %d" rubric_file (!lineno)) )
  )

(* Ensure the rubric file is correctly formatted *)
let assert_valid_rubric () : unit =
  List.iter ~f:(assert_valid_line ()) (read_lines (open_in rubric_file))

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
    let dir_opt = List.fold_left dirs ~init:None ~f:(fun dir_opt new_dir ->
                                                     match dir_opt with
                                                     | Some dir -> dir_opt (* already found a good student, skip rest of directories *)
                                                     | None -> (* Go to student's folder, copy in the tests, compile each, check output *)
                                                        let _ = Sys.chdir new_dir in
                                                        (* Copy the tests over, compile  each *)
                                                        let all_compile = List.fold_left test_suite ~init:true ~f:(fun acc test_name ->
                                                                                                                   acc && (let _ = Sys.command (Format.sprintf "cp %s/%s.ml ." test_dir test_name) in
                                                                                                                           let exit_code = exit_code_of_build test_name in exit_code = 0)
                                                                                                                  ) in
                                                        let _ = Sys.chdir cwd in
                                                        if all_compile then Some new_dir else None
                                                    ) in
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
  let test_names =
    List.rev (List.fold_left
                (read_lines (open_in test_output))
                ~init:[]
                ~f:(fun all_names test_file_full ->
                    let test_file = strip_suffix test_file_full in
                    (* Run the test to print names *)
                    let cmd = Format.sprintf "./_build/%s.d.byte inline-test-runner dummy -list-test-names > %s" test_file test_output in
                    let _ = Sys.command cmd in
                    (* Generated file contains one line per unit test *)
                    let names =
                      List.rev (List.fold_left
                                  test_suite
                                  ~init:[]
                                  ~f:(fun acc line ->
                                      let name = test_name_of_line line in
                                      name :: acc)) in
                    (test_file , names) :: all_names)) in
  let _ = Sys.chdir cwd in
  let chn = open_out rubric_file in
  let response = ref (-1) in
  (* Query for point totals per test *)
  let _ = List.iter ~f:(fun (file_name, unittest_names) ->
                     let _ = Format.printf "\n## Collecting point values for file '%s' ##\n" file_name in
                     let _ = output_string chn (Format.sprintf "# %s\n" file_name) in
                     (* For each unit test, ask the user for a point value & save it *)
                     List.iter ~f:(fun name ->
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
  let _ = flush chn; Out_channel.close chn in
  Format.printf "Successfully recorded rubric in file '%s'\n" rubric_file

(** [dict_of_rubric_file f] read in the yaml-eqsue file [f]. Create a
 * dictionary of (unit_test_name -> (file_name * point_value)) *)
let dict_of_rubric_file (f : string) =
  (* unit_test_name -> (test_file_name * int) dict) *)
  let rubric = Hashtbl.create ~hashable:Hashtbl.Poly.hashable () in
  (* State while parsing file.
   * Track line number and filename *)
  let lineno = ref 0 in
  let _ = List.iter ~f:(fun raw_line ->
    let _ = incr lineno in
    let line = String.strip raw_line in
    if (String.length line) > 0 && line.[0] <> '#' then
      (* Line is hopefully a test. Pretend that it is *)
      let raw_name, raw_points = lsplit line ':' in
      let name = String.strip raw_name in
      let points =
        try int_of_string raw_points with
          | Failure "int_of_string" ->
            raise (Invalid_rubric (Format.sprintf "Rubric '%s' is not valid. Error on line %d\n" rubric_file (!lineno)))
      in
      Hashtbl.add_exn rubric ~key:name ~data:points
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
  let _ = List.iter ~f:(fun name ->
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
  let _ = Out_channel.close chn in
  Format.printf "Finished creating reversed rubric\n%!"

(* Read a dictionary from a file of "test : should_pass : points"
 * sample line would be: "mytest : Y:99" *)
let reverse_dict (rubric_file : string) =
  let d = Hashtbl.create ~hashable:Hashtbl.Poly.hashable () in
  let () = List.iter ~f:(fun line ->
    if not (line.[0] = '#') then
      (* Not a comment. Pls process *)
      let raw_name, rest = lsplit line ':' in
      let raw_should_pass, raw_points = lsplit rest ':' in
      let name = String.strip raw_name in
      let should_pass =
        match (String.strip raw_should_pass).[0] with
          | 'Y' -> true
          | 'N' -> false
          | _ -> raise (Invalid_rubric (Format.sprintf "Cannot tell whether test '%s' should pass or fail in '%s'\n" name rubric_file))
      in
      let points =
        try int_of_string raw_points with
          | Failure "int_of_string" -> raise (Invalid_rubric "Malformed reverse rubric")
      in
      Hashtbl.add_exn d ~key:name ~data:(should_pass, points)
  ) (read_lines (open_in rubric_file)) in
  d

let rubric_command =
  Command.basic
    ~summary:"Create a rubric using the implementations in <sol-dir> to compile the tests."
    ~readme:(fun () -> "TODO")
    Command.Spec.(
    empty
    +> anon ("sol-dir" %: file))
    (fun sol_dir () ->
     assert_file_exists tests_dir;
     let test_suite =
       Array.fold_right (Sys.readdir tests_dir)
                        ~init:[]
                        ~f:(fun f acc -> strip_suffix f :: acc) in
     create_rubric test_suite (get_subdirectories sol_dir))
