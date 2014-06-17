open Constants
open IOUtil

(**** string/filepath utilities ***********************************************)

(** [absolute_path f] Prepend the current working directory to
 * the filepath [f] *)
let absolute_path (f : string) =
  Format.sprintf "%s/%s" (Sys.getcwd ()) f

(** [split s c b] safely splits string [s] at the left/right-most occurence
 * of character [c]. [b] chooses. If [c] does not appear, returns two 
 * copies of [s]. *)
let split (s : string) (c : char) (b : bool) : string * string = 
  if not (String.contains s c) then
    (s, s)
  else 
    let i = 1 + if b then String.index s c else String.rindex s c in
    let left = String.sub s 0 (i-1) in
    let right = String.sub s i ((String.length s) - i) in 
    (String.trim left, String.trim right)

let lsplit (s : string) (c : char) : string * string = 
  split s c true
let rsplit (s : string) (c : char) : string * string =
  split s c false

(** [strip_suffix str] strips all characters after and including the 
 * rightmost period (.) *)
let strip_suffix (filename : string) : string =
  if (String.contains filename '.')
  then String.sub filename 0 (String.rindex filename '.')
  else filename

(** [strip_trailing_slash str] removes the last character of [s],
 * but only if that character is a / *)
let strip_trailing_slash (s : string) =
  let len = String.length s in
  if s.[len-1] = '/'
  then String.sub s 0 (len - 1) 
  else s

(** [strip_trailing_slash_all strs] remove the trailing slash from a 
 * list of files *)
let strip_trailing_slash_all (directories : string list) = 
  List.map (fun s -> strip_trailing_slash s) directories

(** [tag_of_path p] strips all characters up to and including the rightmost / *)
let tag_of_path (path : string) = 
  if String.contains path '/' then 
    let i = 1 + String.rindex path '/' in
    String.sub path i ((String.length path) - i)
  else 
    path

(** [assert_file_exists f] raises [File_not_found] if [f] does not exist *)
let assert_file_exists (filename : string) : unit =
  if not (Sys.file_exists filename) then
    raise (File_not_found filename)

(** [ensure_dir d] creates the directory [d] if it does not exist already. *)
let ensure_dir (dir_name : string) =
  if not (Sys.file_exists dir_name) then 
    Unix.mkdir dir_name 0o777

(**
 * [directories_of_list dir] Build a list of directories from the file [dir].
 * [dir] should contain a newline-separated list of directories.
 * Example: to test files { dir/a , dir/b , dir/c }, you may
 *   > cat "a\nb\nc" > dir/files
 *   > cs3110 smoke @dir/files
 * This is the same as
 *   > cs3110 smoke dir/a dir/b dir/c
 *)
let directories_of_list (dir : string) : string list = 
  let dir = strip_trailing_slash dir in
  let len = String.length dir in
  (* Remove the leading @ to get a filename *)
  let fname = String.sub dir 1 (len-1) in
  let _ = assert_file_exists fname in
  (* Argument is @path/to/list-of-directory-names.    *
   * Extract a list of directory names from the list. *)
  let dir_names = read_lines (open_in fname) in
  let prefix = String.sub fname 0 (String.rindex dir '/') in
  (* Return the fully-inferred list of directories *)
  List.rev (List.fold_left (fun acc name -> (prefix ^ name) :: acc) [] dir_names)

(** [test_name_of_line s] extract the test name from a line printed by the 
 * inline test runner. Name should be the last 'word' of the string, separated
 * from everything else by a colon *)
let test_name_of_line (line : string) : string =
  fst (lsplit (snd (rsplit line ':')) ' ')

(** [get_extension file_name] gets the extension of the file
    [file_name]. The extension is defined to be the characters
    occuring to the right of the right-most occurence of the '.'
    character. *)
let get_extension (file_name : string) : string option =
  try 
    let start  = String.rindex file_name '.' + 1 in
    let length = String.length file_name - start in
    Some (String.sub file_name start length)
  with
    Not_found          -> None
  | Invalid_argument _ -> failwith "get_extension : invalid file extension."

(** [filter_by_extension desired_extension files] returns a list of
    the files in [files] that have the desired extension. *)
let filter_by_extension (desired_extension : string)
                        (files : string list) : string list =
  let has_desired_extension file = match get_extension file with
    | None -> false
    | Some ext -> ext = desired_extension in
  List.filter has_desired_extension files

(** [get_files_with_extension] returns a list containing all of the
    filenames in the given directory that have the desired extension. *)
let get_files_with_extension (desired_extension : string)
                             (dir : string) : string list =
  let get_files dir =
    filter_by_extension desired_extension (Array.to_list (Sys.readdir dir)) in
  try get_files dir with _ -> []

