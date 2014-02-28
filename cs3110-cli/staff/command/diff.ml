open Constants
open Core.Std
open FilepathUtil
open IOUtil

exception InvalidDirectory of string

(** Where the temporary results of an individual diff are stored. *)
let diff_tmp = ".diff"

(** It is fascinating that Jane Street didn't include this in Core... *)
let array_mem xs x = 
  try let _ = Array.find_exn xs ~f:((=) x) in true
  with Not_found -> false

(** Again... are you serious? How the hell is this not in Core? *)
let list_of_array xs = Array.fold_right ~f:(fun x a -> x::a) xs ~init:[]

(** Gets the subset of the submissions that did not compile. This is
    just the intersection of the names in [submission_dir] and
    [nocompile_dir]. Raises [InvalidDirectory] if either of the
    directories is invalid. *)
let get_nocompiles submission_dir nocompile_dir =
  match Sys.is_directory submission_dir, Sys.is_directory nocompile_dir with
  (** Submission directory is invalid. *)
  | `No,`No      | `No,`Unknown      | `No,`Yes
  | `Unknown,`No | `Unknown,`Unknown | `Unknown,`Yes -> begin
    let msg = Printf.sprintf
      "The desired submission directory:\n\t%s\nis invalid." submission_dir in
    raise (InvalidDirectory msg)
  end
  (** No compile directory is invalid. *)  
  | `Yes,`No | `Yes,`Unknown -> begin
    let msg = Printf.sprintf
      "The nocompile directory:\n\t%s\nis invalid." nocompile_dir in
    raise (InvalidDirectory msg)
  end
  (** Both good. We goin' in. *)
  | `Yes, `Yes -> begin
    let submissions = Sys.readdir submission_dir in
    let nocompiles  = Sys.readdir nocompile_dir  in
    Array.filter ~f:(array_mem nocompiles) submissions
  end

let rec prompt_user () =
  print_endline
    "** Should the student be penalized for the late submission? y/n\n";
  match read_line () with
  | "y" | "Y" | "yes" | "Yes" -> 0
  | "n" | "N" | "no"  | "No"  -> 1
  | _ -> begin
    print_endline "Invalid input! Please enter 'yes' or 'no'.";
    prompt_user ()
  end

(** Runs the system 'diff' command on [orignal_file] and [new_file],
    in that order. The output of the command is put into the
    [diff_tmp] directory. Returns [1] if the student receives credit
    and [0] if not. *)
let diff_files original_file new_file =
  let err_msg f =
    let msg = Printf.sprintf "The file:\n\t%s\nis invalid." f in
    raise (InvalidDirectory msg) in
  match Sys.file_exists original_file, Sys.file_exists new_file with
  (** [original_file] is not there... *)
  | `No,`No      | `No,`Unknown      | `No,`Yes
  | `Unknown,`No | `Unknown,`Unknown | `Unknown,`Yes -> err_msg original_file
  (** [new_file] is not there. This means that the student did not
      resubmit. Do not penalize them. *)
  | `Yes,`No | `Yes,`Unknown -> begin
    print_endline (String.concat ~sep:"" [
      "The file:\n";
      "\t"^new_file^"\n";
      "does not exist. This means student did not resubmit the file:";
      "\t"^original_file^"\n";
      "No penalty deducted.\n"
    ]);
    1
  end
  (** Both good. Turn up *)
  | `Yes,`Yes -> begin
    let cmd = Printf.sprintf "diff %s %s" original_file new_file in
    Printf.printf "\n### Executing '%s' ###\n" cmd;
    ignore(Sys.command (Printf.sprintf "%s > %s" cmd diff_tmp));
    match read_lines (open_in diff_tmp) with
    (** No differences *)
    | [] -> begin
      print_endline "\'diff\' exited with no differences.";
      1
    end
    | _ as lines -> begin
      (** Non-empty diff. Display it and wait for the user response. *)
      List.iter lines ~f:print_endline;
      prompt_user ()
    end
  end

(** [diff_student] runs the [diff] process on one student. It compares
    each of the new submissions to the old ones, in sequence, until an
    annacceptable diff has been encountered. Returns [1] if the
    student is OK and [0] otherwise. *)
let diff_student submit_dir resubmit_dir =
  match Sys.is_directory submit_dir, Sys.is_directory nocompile_dir with
  (** Submission directory is invalid. *)
  | `No,`No      | `No,`Unknown      | `No,`Yes
  | `Unknown,`No | `Unknown,`Unknown | `Unknown,`Yes -> begin
    let msg = Printf.sprintf
      "The desired submission directory:\n\t%s\nis invalid." submit_dir in
    raise (InvalidDirectory msg)
  end
  (** Resubmission directory is invalid. *)  
  | `Yes,`No | `Yes,`Unknown -> begin
    let msg = Printf.sprintf
      "The resubmission directory:\n\t%s\nis invalid." nocompile_dir in
    raise (InvalidDirectory msg)
  end
  (** Going HAM *)
  | `Yes, `Yes -> begin
    Array.fold_right (Sys.readdir submit_dir) ~f:(fun file a ->
      if a=0 then 0 else begin
        diff_files file (Printf.sprintf "%s/%s" resubmit_dir file)
      end) ~init:1
  end 


(** [diff]'s each student in the nocompile directory in sequence. This
    function interactively queries the user for input to see whether
    or not a resubmitted file deserves credit. The results for each
    student are stored in the [diff_results] file, which can be
    uploaded to CMS. *)
let diff submissions () =
  match Sys.is_directory submissions with
  | `No | `Unknown -> begin
    print_endline (String.concat ~sep:"\n" [
      "The directory:";
      "\t"^submissions;
      "is invalid. Please input a valid submissions directory.\n"
    ]);
    exit 1
  end
  | `Yes -> begin
    try
      (** This map maps netids -> submission_dir. *)
      let netid_map = String.Table.create () ~size:16 in
      (** Populate the map with the submissions. *)
      List.iter
        ~f:(fun dir -> Hashtbl.set netid_map ~key:(tag_of_path dir) ~data:dir)
        (strip_trailing_slash_all (list_of_array (Sys.readdir submissions)));
      let output_channel = Out_channel.create diff_results in
      let students_to_diff = get_nocompiles submissions "./_nocompile" in
      let total_diffed =
        Array.fold_right
          students_to_diff
          ~f:(fun netid num_diffed ->
            let submit_dir   = Printf.sprintf "%s/%s" submissions netid in
            let resubmit_dir = Printf.sprintf "%s/%s" "./_nocompile" netid in
            let result = diff_student submit_dir resubmit_dir in
            Out_channel.output_string
              output_channel (Printf.sprintf "%s,%d\n" netid result);
            num_diffed+1)
          ~init:0 in
      (** Clean up temporary files. *)
      ignore(Sys.command (Printf.sprintf "rm -f %s" diff_tmp));
      (** Close the file that we wrote the results to. *)
      Out_channel.close output_channel;
      Printf.printf
        "Finished diffing %d files. See '%s' for results.\n"
        total_diffed
        diff_results
    with
    | InvalidDirectory errmsg -> print_endline errmsg; exit 1
    | _ -> begin
      print_endline "Diff : an unexpected error occured. Try again.";
      exit 1
    end
  end

let diff_command =
  Command.basic
    ~summary:"Runs a diff on each student and aks the user for judgment."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "The [cs3110 diff] command run the usual UNIX 'diff' command on the";
      "set of students that resubmitted their code after the deadline. It";
      "starts up an interactive prompt where the user can read the diff results";
      "of each file individually and determine whether to cost the student a";
      "slip day or not. The results are recorded in a .csv file, which can";
      "subsequently be uploaded to CMS."
    ])
    Command.Spec.(empty +> anon ("submission_dir" %: file))
    diff

let run_diff () =
  Command.run ~version:"2.0" ~build_info:"Core" diff_command
