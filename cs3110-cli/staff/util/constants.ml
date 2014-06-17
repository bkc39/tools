(**** exceptions and constants ************************************************)
exception File_not_found of string
exception Invalid_rubric of string

let expected_version = "4.01.0"
let email_subject = "[CS3110 test harness] compile error"
let email_admins = "_email_bcc.txt"

let cms_dir = "./_cms"
let cms_fname = Format.sprintf "%s/spreadsheet.csv" cms_dir
let depend_file = "./.depend"
let diff_results = "_diff_results.csv"
let email_dir = "./_email"
let lib_file = "./.libs"
let nocompile_dir = "./_nocompile"
let opam_packages_file = "./.opam_packages"
let output_dir = "./_output"
let reverse_cms = Format.sprintf "%s/reverse.csv" cms_dir
let reverse_dir = "./reverse_tests"
let reverse_rubric = "./reverse_rubric.yaml"
let rubric_file = "./rubric.yaml"
let smoke_targets = "./smoke_test"
(* std_opam_packages may NOT be empty! Need pa_ounit, at least, to compile *)
let std_opam_packages = ["pa_ounit.syntax"; "oUnit"; "pa_ounit"; "qcheck"]
let tests_dir = "./tests"

let test_output = "inline_tests.log"
let fail_output = "inline_test_failures.log"
