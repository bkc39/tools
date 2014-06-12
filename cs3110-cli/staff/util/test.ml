open Constants
open FilepathUtil

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
