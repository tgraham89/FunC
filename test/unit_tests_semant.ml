open Sast

(*

unit_tests_semant.ml

Runs UTs against various test cases defined in the happy_test_inputs or unhappy_test_inputs folder.

This file additionally produces pretty output, which can be seen in the `unit_tests_semant.out` file in the project dir.
This shows you what test ran, and that it executed as expected.
It will also print out the resulting error thrown so you know exactly how the test failed.

What the test does:
1. Read input file.
2. Feed the file to scanner, parser as usual.
3. Semantically check the output.
4. If successful, returns a string program.
5. Assert that no error was thrown for happy tests, and semantic error was thrown for unhappy tests.
*)




(* Input string arg, returns a valid parsed string
or errors out if it cannot be parsed. *)
let feed_semant str_input =
  let lexbuf = Lexing.from_channel str_input in
  let program = Parser.program_rule Scanner.token lexbuf in
  let sprogram = Semant.check program in
  string_of_sprogram sprogram

(* Opens the file and passes it into the semant for work.
Checks that the resulting string program isn't empty.
If it looks good returns true.
In any error case, returns false. *)
let check_exception_was_raised file_name =
  try
    let opened_file = open_in file_name in
    let string_program = feed_semant opened_file in
    assert (String.length string_program != 0);
    close_in opened_file;
    true
  with
    Failure ex -> print_endline ("\nException occurred during the test: " ^ ex); false
    | _ -> print_endline ("\nProgram was not generated due to parsing failure."); false


(* Runs test and asserts result *)
let run_test file_name expected_result : unit =
  let result = check_exception_was_raised file_name in
  print_endline ("Testing file: " ^ file_name
                 ^ " with result: " ^ string_of_bool result
                 ^ ". Expected value should be: " ^ string_of_bool expected_result);
  assert (result = expected_result);
  ()

(* File processing func *)
let process_file dir expect file : unit =
  let file_name = Filename.concat dir file in
  run_test file_name expect  

(* Helper funcs ^^^^^ *)





(* Test funcs vvvvvv *)

(* Series of happy path tests (these should NOT error out) *) 
let run_happy_path_tests () =
  let happy_past_test_dir = "test/happy_test_inputs/parser_and_semant" in
  let test_files = Sys.readdir happy_past_test_dir in
  Array.iter (process_file happy_past_test_dir true) test_files

(* Series of unhappy path tests (these should error out) *) 
let run_unhappy_path_tests () =
  let unhappy_past_test_dir = "test/unhappy_test_inputs/semant" in
  let test_files = Sys.readdir unhappy_past_test_dir in
  Array.iter (process_file unhappy_past_test_dir false) test_files


(* Runs all tests *) 
let run_tests () =
  run_happy_path_tests ();
  run_unhappy_path_tests ()

(* Execute test suite for semant file *)
(* These tests are read from the test inputs folder. *)
let () =
  run_tests ();
  print_endline "\nunit_tests_semant.ml passed.\n"
