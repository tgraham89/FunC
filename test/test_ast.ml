open Ast

(* All operators-- these should always work *)
let run_op_tests () =
  let op1 = Add in
  let op2 = Sub in
  let op3 = Equal in
  let op4 = Neq in
  let op5 = Less in
  let op6 = Lequal in
  let op7 = And in
  let op8 = Or in
  let op9 = Not in
  let op10 = Greater in
  let op11 = Gequal in
  let op12 = Mult in
  let op13 = Div in
  let op14 = Mod in
  let op15 = Vbar in
  assert (string_of_op op1 = "+");
  assert (string_of_op op2 = "-");
  assert (string_of_op op3 = "==");
  assert (string_of_op op4 = "!=");
  assert (string_of_op op5 = "<");
  assert (string_of_op op6 = "<=");
  assert (string_of_op op7 = "&&");
  assert (string_of_op op8 = "||");
  assert (string_of_op op9 = "!");
  assert (string_of_op op10 = ">");
  assert (string_of_op op11 = ">=");
  assert (string_of_op op12 = "*");
  assert (string_of_op op13 = "/");
  assert (string_of_op op14 = "%");
  assert (string_of_op op15 = "|")


(* Bare minimum test cases that should always work *)
let run_expr_tests () =
  let expr1 = Zero in
  let expr2 = Literal 1 in
  let expr3 = BoolLit(true) in
  let expr4 = BoolLit(false) in
  let expr5 = StrLit("hello world") in
  let expr6 = ChrLit('c') in
  let expr7 = FloatLit(9.99) in
  let expr8 = ListLit([Literal 1]) in
  let expr9 = ListLit([Literal 1; Literal 2; Literal 3]) in
  let expr10 = ListLit([]) in
  let expr11 = Struct([Literal 1]) in
  let expr12 = Struct([Literal 1; Literal 2; Literal 3]) in
  let expr13 = Struct([]) in
  let expr14 = Id("id") in
  let expr15 = Binop(Literal 1, Add, Literal 1) in
  let expr16 = Assign("x", StrLit("hello world")) in
  assert (string_of_expr expr1 = "0");
  assert (string_of_expr expr2 = "1");
  assert (string_of_expr expr3 = "true");
  assert (string_of_expr expr4 = "false");
  assert (string_of_expr expr5 = "\"hello world\"");
  assert (string_of_expr expr6 = "c");
  assert (string_of_expr expr7 = "9.99");
  assert (string_of_expr expr8 = "[1]");
  assert (string_of_expr expr9 = "[1, 2, 3]");
  assert (string_of_expr expr10 = "[]");
  assert (string_of_expr expr11 = "{1}");
  assert (string_of_expr expr12 = "{1, 2, 3}");
  assert (string_of_expr expr13 = "{}");
  assert (string_of_expr expr14 = "id");
  assert (string_of_expr expr15 = "1 + 1");
  assert (string_of_expr expr16 = "x = \"hello world\"")


(* Edge cases for expression tests *)
(* TODO: these look wrong today? *)
let run_expr_tests_edge_cases () =
  let list_with_multiple_types = ListLit([Literal 1; BoolLit(true)]) in
  let struct_with_multiple_types = Struct([Literal 1; BoolLit(true)]) in
  let id_with_quotes = Id("\"quotation marks\"") in
  assert (string_of_expr list_with_multiple_types = "[1, true]");
  assert (string_of_expr struct_with_multiple_types = "{1, true}");
  assert (string_of_expr id_with_quotes = "\"quotation marks\"")

(* Test cases for assignment *)
(* TODO: these also look wrong today, need to check with team *)
let run_expr_tests_function_cases () =
  let hello_world_func = Function([Decl(Int, "hello_world_func")], [Expr(StrLit("hello world"))]) in
  let defn_func = Function([Defn(Int, "my_func", Assign("my_var", StrLit("my_val")))], [Expr(StrLit("hello world"))]) in
  assert (string_of_expr hello_world_func = "(int hello_world_func, ) {\n\t;\n\t\"hello world\";\n}");
  assert (string_of_expr defn_func = "(int my_func = my_var = \"my_val\", ) {\n\t;\n\t\"hello world\";\n}")

(* Test cases for FuncInvoc *)
let run_expr_tests_func_invoc_cases () =
  let basic_invocation = FuncInvoc("my_first_func_invoc", [Literal 1; Literal 2; Literal 3]) in
  let multiple_arg_invocation = FuncInvoc("multiple_types_of_args", [Literal 1; BoolLit(true); ChrLit('c')]) in
  assert (string_of_expr basic_invocation = "my_first_func_invoc(1, 2, 3)");
  assert (string_of_expr multiple_arg_invocation = "multiple_types_of_args(1, true, c)")


(* Runs all tests... look at each function for the actual cases *)
(* TODO: need to add test cases for type,  bind, statement *)
let run_tests () =
  run_op_tests ();
  run_expr_tests ();
  run_expr_tests_edge_cases ();
  run_expr_tests_function_cases ();
  run_expr_tests_func_invoc_cases ()

(* Execute test suite for ast.ml *)
let () =
  run_tests ();
  print_endline "ast_test.ml passed"
