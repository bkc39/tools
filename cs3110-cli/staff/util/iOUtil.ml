(**** i/o utilities ***********************************************************)

(** [read_lines c] reads all lines from channel [c] into a list *)
let read_lines (chn : in_channel) : string list =
  let rec read_aux (chn : in_channel) (out : string list) : string list =
    try 
      let output' = input_line chn :: out in
      read_aux chn output'
    with End_of_file -> 
      let _ = close_in chn in 
      out
  in
  List.rev (read_aux chn [])

(** [csv_of_file filename] reads all lines in [filename] and concatenates them
 * with commas *)
let csv_of_file (filename : string) : string =
  let chn = open_in filename in
  String.concat "," (read_lines chn)

(** The [do_if_directory] function is used to process directories. So
    [do_if_directory dir f err_msg ~exit_code:n] executes the process
    [f] if [dir] is a valid directory and prints the error message
    [err_msg] to standard out and exits the current process with exit
    code [n] if not. The default exit code is 1. *)
let do_if_directory (dir : string)
                    (f : string -> 'a)
                    (err_msg : string)
                    (exit_code : int) : 'a =
  if Sys.file_exists dir && Sys.is_directory dir then f dir
  else prerr_endline err_msg; exit exit_code
