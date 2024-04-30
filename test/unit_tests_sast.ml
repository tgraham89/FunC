open Sast
open Ast

(* 
These are unit tests that cover
the sast file via the sast stringify functions
*)


(*
run_test helper func, that prints expectation and result.
*)
let run_test test_arg expected_result =
  try
    print_endline ("Checking: " ^ string_of_sexpr test_arg ^ " to equal: " ^ expected_result);
    assert (string_of_sexpr test_arg = expected_result);
    print_endline ("Passed.");
    true
  with
    _ -> print_endline ("Failed."); false


(*
^^ Helper funcs ^^


vv Test  funcs  vv
*)


let run_zero_test () =
  let arg1 = (Int, SZero) in
  let expected1 = "(int : 0)" in
  assert (run_test arg1 expected1)

let run_int_literal_test () = 
  let arg1 = (Int, SLiteral(1)) in
  let expected1 = "(int : 1)" in
  assert (run_test arg1 expected1)

let run_bool_literal_test () = 
  let arg1 = (Bool, SBoolLit(true)) in
  let expected1 = "(bool : true)" in
  assert (run_test arg1 expected1)

let run_string_literal_test () = 
  let arg1 = (String, SStrLit("hello world")) in
  let expected1 = "(string : \"hello world\")" in
  assert (run_test arg1 expected1)

let run_char_literal_test () = 
  let arg1 = (Char, SChrLit('c')) in
  let expected1 = "(char : c)" in
  assert (run_test arg1 expected1)

let run_float_literal_test () = 
  let arg1 = (Float, SFloatLit(0.123)) in
  let expected1 = "(float : 0.123)" in
  assert (run_test arg1 expected1)

let run_id_literal_test () = 
  let arg1 = (String, SId("columbia ID")) in
  let expected1 = "(string : columbia ID)" in
  assert (run_test arg1 expected1)

let run_sunary_op_tests () =
  let positive_arg = (Int, SUnaryOp(SPos, (Int, SLiteral(1)))) in
  let positive_exp = "(int : +(int : 1))" in
  let negative_arg = (Int, SUnaryOp(SNeg, (Int, SLiteral(1)))) in
  let negative_exp = "(int : -(int : 1))" in
  assert (run_test positive_arg positive_exp);
  assert (run_test negative_arg negative_exp)




(* 

type sunary_operator = SNeg | SPos

type sexpr = typ * sx
and sx =


  | SBinop of sexpr * bop * sexpr
  | SAssign of string * sexpr
  | SListLit of typ * sexpr list
  | SFunction of sbind list * sstmt list
  | SCall of sexpr * sexpr list
  | SStructId of string
  | SStructAccess of sexpr
  | SStructAssign of sexpr list (* Used to define an instance of a struct *)
and sbind =
  | SDecl of typ * string
  | SDefn of typ * string * sexpr
and sstmt =
  | SBlock of sstmt list
  | SExpr of sexpr
  | SBind of sbind
  | SIf of sexpr * sstmt
  | SIfElse of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | SFor of sbind * sexpr * sexpr * sstmt
  | SReturn of sexpr
  | SStructDecl of {
        sname: string;
        members: sbind list;
        }
and program = {
  sbody: sstmt list;
}
 *)



(* Runs tests against all sexpr *)

let run_sexpr_tests () =
  run_zero_test ();
  run_int_literal_test ();
  run_bool_literal_test ();
  run_string_literal_test ();
  run_char_literal_test ();
  run_float_literal_test ();
  run_id_literal_test ();
  run_sunary_op_tests()

(* Runs all tests... look at each function for the actual cases *)
let run_tests () =
  run_sexpr_tests ()

(* Execute test suite for ast.ml *)
let () =
  run_tests ();
  print_endline "unit_tests_ast.ml passed"
