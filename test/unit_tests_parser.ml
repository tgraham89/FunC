open Ast

(*

unit_tests_parser.ml

Runs UTs against various test cases defined in the happy_test_inputs or unhappy_test_inputs folder.

This file additionally produces pretty output, which can be seen in the `unit_tests_parser.out` file in the project dir.
This shows you what test ran, and that it executed as expected.

What the test does:
1. Read input file.
2. Feed to parser.
3. Convert parser result to string.
4. Assert that no error was thrown for happy tests, and parsing error was thrown for unhappy tests.

*)




(* Input string arg, returns a valid parsed string
or errors out if it cannot be parsed. *)
let feed_parser str_input =
  let lexbuf = Lexing.from_channel str_input in
  let program = Parser.program_rule Scanner.token lexbuf in
  string_of_program program

(* Opens the file and passes it into the parser for work.
Checks that the resulting string program isn't empty.
If it looks good returns true.
In any error case, returns false. *)
let check_exception_was_raised file_name =
  try
    let opened_file = open_in file_name in
    let string_program = feed_parser opened_file in
    assert (String.length string_program != 0);
    close_in opened_file;
    true
  with
    _ -> false


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
  let happy_past_test_dir = "test/happy_test_inputs" in
  let test_files = Sys.readdir happy_past_test_dir in
  Array.iter (process_file happy_past_test_dir true) test_files

(* Series of unhappy path tests (these should error out) *) 
let run_unhappy_path_tests () =
  let unhappy_past_test_dir = "test/unhappy_test_inputs" in
  let test_files = Sys.readdir unhappy_past_test_dir in
  Array.iter (process_file unhappy_past_test_dir false) test_files


(* Runs all tests *) 
let run_tests () =
  run_happy_path_tests ();
  run_unhappy_path_tests ()

(* Execute test suite for parser file *)
(* These tests are read from the test inputs folder. *)
let () =
  run_tests ();
  print_endline "unit_tests_parser.ml passed"
