open Constants
open Core.Std
open FilepathUtil
open IOUtil

type email_result = [ `Success | `Failure ]

let email_student msg_file =
  let recipient = Printf.sprintf "%s@cornell.edu" (strip_suffix msg_file) in
  let bcc_addresses =
    String.concat ~sep:" -b "
      (In_channel.with_file email_admins ~f:read_lines) in
  let bcc = Printf.sprintf "-b %s" bcc_addresses in
  let cmd =
    Printf.sprintf
      "mutt -s '%s' %s '%s' < %s/%s"
      email_subject
      bcc
      recipient
      email_dir
      msg_file in
  Printf.printf "### Executing %s\n%!" cmd;
  let exit_code = Sys.command cmd in
  if exit_code = 0 then `Success else begin
    Printf.printf "Failed to send message to: %s\n" recipient;
    `Failure
  end

let email () =
  assert_file_exists email_dir;
  assert_file_exists email_admins;
  let messages = Sys.readdir email_dir in
  (** Use the [mail] command to package off the email message *)
  let total_success, total_failed =
    Array.fold_right
      ~f:(fun msg_file (num_success,num_failed) ->
        match email_student msg_file with
        | `Success -> (num_success+1,num_failed)
        | `Failure -> (num_success,num_failed+1))
      (Sys.readdir email_dir)
      ~init:(0,0) in
  let output_string =
    let open Printf in
    String.concat ~sep:"\n" [
      sprintf "Attempted to send %d messages."    (total_success+total_failed);
      sprintf "\t%d messages sent successufully." total_success;
      sprintf "\t%d messages failed to send."     total_failed
    ] in
  print_endline output_string

let email_command =
  Command.basic
    ~summary:(String.concat ~sep:"\n" [
      "Emails all of the students in [email_dir] with a message indicating";

      "that they did not compile."
    ])
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "Gathers each of the netid's in [email_dir] and emails them a message.";
      "All of the administrators specidied in the [email_admins] file are";
      "included as bcc recipents of the message. It also prints the set of";
      "netid's for whom the message failed to send to standard out."
    ])
    Command.Spec.empty
    email

let run_email_command () =
  Command.run ~version:"2.0" ~build_info:"Core" email_command
