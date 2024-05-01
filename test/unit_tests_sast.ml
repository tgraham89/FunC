open Sast
open Ast

(* 
These are unit tests that cover
the sast file via the sast stringify functions
*)


let run_sexpr_test test_arg expected_result =
  try
    print_endline ("Checking: " ^ string_of_sexpr test_arg ^ " to equal: " ^ expected_result);
    assert (string_of_sexpr test_arg = expected_result);
    print_endline ("Passed.");
    true
  with
    _ -> print_endline ("Failed."); false

let run_sbind_test test_arg expected_result =
  try
    print_endline ("Checking: " ^ string_of_sbind test_arg ^ " to equal: " ^ expected_result);
    assert (string_of_sbind test_arg = expected_result);
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
  assert (run_sexpr_test arg1 expected1)

let run_int_literal_test () = 
  let arg1 = (Int, SLiteral(1)) in
  let expected1 = "(int : 1)" in
  assert (run_sexpr_test arg1 expected1)

let run_bool_literal_test () = 
  let arg1 = (Bool, SBoolLit(true)) in
  let expected1 = "(bool : true)" in
  assert (run_sexpr_test arg1 expected1)

let run_string_literal_test () = 
  let arg1 = (String, SStrLit("hello world")) in
  let expected1 = "(string : \"hello world\")" in
  assert (run_sexpr_test arg1 expected1)

let run_char_literal_test () = 
  let arg1 = (Char, SChrLit('c')) in
  let expected1 = "(char : c)" in
  assert (run_sexpr_test arg1 expected1)

let run_float_literal_test () = 
  let arg1 = (Float, SFloatLit(0.123)) in
  let expected1 = "(float : 0.123)" in
  assert (run_sexpr_test arg1 expected1)

let run_id_literal_test () = 
  let arg1 = (String, SId("columbia ID")) in
  let expected1 = "(string : columbia ID)" in
  assert (run_sexpr_test arg1 expected1)

let run_sunary_op_tests () =
  let positive_arg = (Int, SUnaryOp(SPos, (Int, SLiteral(1)))) in
  let positive_exp = "(int : +(int : 1))" in
  let negative_arg = (Int, SUnaryOp(SNeg, (Int, SLiteral(1)))) in
  let negative_exp = "(int : -(int : 1))" in
  assert (run_sexpr_test positive_arg positive_exp);
  assert (run_sexpr_test negative_arg negative_exp)

let run_binop_tests () =
  let arg1 = (Int, SBinop((Int, SLiteral(1)), Add, (Int, SLiteral(1)))) in
  let expect1 = "(int : (int : 1) + (int : 1))" in
  let arg2 = (Int, SBinop((Int, SLiteral(1)), Sub, (Int, SLiteral(1)))) in
  let expect2 = "(int : (int : 1) - (int : 1))" in
  let arg3 = (Int, SBinop((Int, SLiteral(1)), Equal, (Int, SLiteral(1)))) in
  let expect3 = "(int : (int : 1) == (int : 1))" in
  let arg4 = (Int, SBinop((Int, SLiteral(1)), Neq, (Int, SLiteral(1)))) in
  let expect4 = "(int : (int : 1) != (int : 1))" in
  let arg5 = (Int, SBinop((Int, SLiteral(1)), Less, (Int, SLiteral(1)))) in
  let expect5 = "(int : (int : 1) < (int : 1))" in
  let arg6 = (Int, SBinop((Int, SLiteral(1)), Lequal, (Int, SLiteral(1)))) in
  let expect6 = "(int : (int : 1) <= (int : 1))" in
  let arg7 = (Int, SBinop((Int, SLiteral(1)), And, (Int, SLiteral(1)))) in
  let expect7 = "(int : (int : 1) && (int : 1))" in
  let arg8 = (Int, SBinop((Int, SLiteral(1)), Or, (Int, SLiteral(1)))) in
  let expect8 = "(int : (int : 1) || (int : 1))" in
  let arg9 = (Int, SBinop((Int, SLiteral(1)), Not, (Int, SLiteral(1)))) in
  let expect9 = "(int : (int : 1) ! (int : 1))" in
  let arg10 = (Int, SBinop((Int, SLiteral(1)), Greater, (Int, SLiteral(1)))) in
  let expect10 = "(int : (int : 1) > (int : 1))" in
  let arg11 = (Int, SBinop((Int, SLiteral(1)), Gequal, (Int, SLiteral(1)))) in
  let expect11 = "(int : (int : 1) >= (int : 1))" in
  let arg12 = (Int, SBinop((Int, SLiteral(1)), Mult, (Int, SLiteral(1)))) in
  let expect12 = "(int : (int : 1) * (int : 1))" in
  let arg13 = (Int, SBinop((Int, SLiteral(1)), Div, (Int, SLiteral(1)))) in
  let expect13 = "(int : (int : 1) / (int : 1))" in
  let arg14 = (Int, SBinop((Int, SLiteral(1)), Mod, (Int, SLiteral(1)))) in
  let expect14 = "(int : (int : 1) % (int : 1))" in
  let arg15 = (Int, SBinop((Int, SLiteral(1)), Vbar, (Int, SLiteral(1)))) in
  let expect15 = "(int : (int : 1) | (int : 1))" in
  let arg16 = (Int, SBinop((Int, SLiteral(1)), Dot, (Int, SLiteral(1)))) in
  let expect16 = "(int : (int : 1) . (int : 1))" in
  assert (run_sexpr_test arg1 expect1);
  assert (run_sexpr_test arg2 expect2);
  assert (run_sexpr_test arg3 expect3);
  assert (run_sexpr_test arg4 expect4);
  assert (run_sexpr_test arg5 expect5);
  assert (run_sexpr_test arg6 expect6);
  assert (run_sexpr_test arg7 expect7);
  assert (run_sexpr_test arg8 expect8);
  assert (run_sexpr_test arg9 expect9);
  assert (run_sexpr_test arg10 expect10);
  assert (run_sexpr_test arg11 expect11);
  assert (run_sexpr_test arg12 expect12);
  assert (run_sexpr_test arg13 expect13);
  assert (run_sexpr_test arg14 expect14);
  assert (run_sexpr_test arg15 expect15);
  assert (run_sexpr_test arg16 expect16)

let run_assign_test () =
  let arg1 = (String, SAssign("x", (String, SStrLit("hello world")))) in
  let exp1 = "(string : x = (string : \"hello world\"))" in
  assert (run_sexpr_test arg1 exp1)

let run_list_test () =
  let arg1 = (List(Int), SListLit(List(Int), [(Int, SLiteral(1)); (Int, SLiteral(2)); (Int, SLiteral(3));])) in
  let exp1 = "(list<int> : ( list of list<int>: [(int : 1), (int : 2), (int : 3)])" in
  assert (run_sexpr_test arg1 exp1)

let run_function_tests () =
  let single_arg_fun = (Int, SFunction([SDecl(String, "func_arg")], [SExpr(String, SStrLit("input string 123"))])) in
  let single_arg_exp = "(int : (string func_arg) {\n\t(string : \"input string 123\");\n\t\n})" in
  let multiple_arg_fun = (Int, SFunction([SDecl(Int, "x1"); SDecl(Char, "x2")], [SExpr(Int, SLiteral(1)); SExpr(Char, SChrLit('c'))])) in
  let multiple_arg_exp = "(int : (int x1, char x2) {\n\t(int : 1);\n\t(char : c);\n\t\n})" in
  assert (run_sexpr_test single_arg_fun single_arg_exp);
  assert (run_sexpr_test multiple_arg_fun multiple_arg_exp)

let run_call_tests () =
  let no_arg_call = (Void, SCall((Int, SLiteral(1)), [])) in
  let no_arg_exp  = "(void : (int : 1)())" in
  let single_arg_call = (Void, SCall((Int, SLiteral(1)), [(Int, SLiteral(1))])) in
  let single_arg_exp  = "(void : (int : 1)((int : 1)))" in
  let multiple_arg_call = (Void, SCall((Int, SLiteral(1)), [(Int, SLiteral(1));(Int, SLiteral(2));(Int, SLiteral(3))])) in
  let multiple_arg_exp  = "(void : (int : 1)((int : 1), (int : 2), (int : 3)))" in
  assert (run_sexpr_test no_arg_call no_arg_exp);
  assert (run_sexpr_test single_arg_call single_arg_exp);
  assert (run_sexpr_test multiple_arg_call multiple_arg_exp)


let run_struct_tests () =
  let struct_id_arg1 = (String, SStructId("struct_id")) in
  let struct_id_exp1 = "(string : struct_id)" in
  let struct_access_arg1 = (String, SStructAccess((String, SStrLit("struct_access")))) in
  let struct_access_exp1 = "(string : (string : \"struct_access\"))" in
  let struct_assign_arg1 = (String, SStructAssign([(String, SStrLit("x1")); (String, SStrLit("x2")); (String, SStrLit("x3"))])) in
  let struct_assign_exp1 = "(string : ((string : \"x1\"), (string : \"x2\"), (string : \"x3\")))" in
  assert (run_sexpr_test struct_id_arg1 struct_id_exp1);
  assert (run_sexpr_test struct_access_arg1 struct_access_exp1);
  assert (run_sexpr_test struct_assign_arg1 struct_assign_exp1)

let run_sdecl_test () =
  let arg1 = (SDecl(String, "sdecl_test")) in
  let exp1 = "string sdecl_test" in
  assert (run_sbind_test arg1 exp1)

let run_sdefn_test () =
  let arg1 = (SDefn(String, "sdefn_test1", (String, SStrLit("sdefn_test2")))) in
  let exp1 = "string sdefn_test1 = (string : \"sdefn_test2\")" in
  assert (run_sbind_test arg1 exp1)

(* 


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
  run_sunary_op_tests ();
  run_binop_tests ();
  run_assign_test ();
  run_list_test ();
  run_function_tests ();
  run_call_tests ();
  run_struct_tests ()

(* Run tests against all sbind *)
let run_sbind_tests () =
  run_sdecl_test ();
  run_sdefn_test ()


(* Runs all tests... look at each function for the actual cases *)
let run_tests () =
  run_sexpr_tests ();
  run_sbind_tests ()

(* Execute test suite for ast.ml *)
let () =
  run_tests ();
  print_endline "unit_tests_ast.ml passed"
