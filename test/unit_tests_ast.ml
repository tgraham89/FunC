open Ast

(* 
These are unit tests that cover
the ast file via the ast stringify functions
*)

(* It's useful to print output and check the .out file to debug broken UTs *)
(* e.g., with `print_endline (string_of_expr expr13)` *)



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
  let op16 = Dot in
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
  assert (string_of_op op15 = "|");
  assert (string_of_op op16 = ".")


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
  let expr11 = StructId("struct id") in
  let expr12 = StructAccess(Literal 1) in
  let expr13 = StructAssign([Literal 1; Literal 2]) in
  let expr14 = Id("id") in
  let expr15 = Binop(Literal 1, Add, Literal 1) in
  let expr16 = Assign("x", StrLit("hello world")) in
  let expr17 = UnaryOp(Pos, Literal 1) in
  let expr18 = UnaryOp(Neg, Literal 1) in
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
  assert (string_of_expr expr11 = "struct id");
  assert (string_of_expr expr12 = "1");
  assert (string_of_expr expr13 = "{\n1,\n2,\n}");
  assert (string_of_expr expr14 = "id");
  assert (string_of_expr expr15 = "1 + 1");
  assert (string_of_expr expr16 = "x = \"hello world\"");
  assert (string_of_expr expr17 = "+(1)");
  assert (string_of_expr expr18 = "-(1)")


(* Edge cases for expression tests *)
let run_expr_tests_edge_cases () =
  let list_with_multiple_types = ListLit([Literal 1; BoolLit(true)]) in
  let id_with_quotes = Id("\"quotation marks\"") in
  assert (string_of_expr list_with_multiple_types = "[1, true]");
  assert (string_of_expr id_with_quotes = "\"quotation marks\"")

(* Test cases for function and bind *)
let run_expr_tests_function_and_bind_cases () =
  let hello_world_func = Function([Decl(Int, "hello_world_func")], [Expr(StrLit("hello world"))]) in
  let defn_func = Function([Defn(Int, "my_func", Assign("my_var", StrLit("my_val")))], [Expr(StrLit("hello world"))]) in
  assert (string_of_expr hello_world_func = "(int hello_world_func) {\n\"hello world\";\n}");
  assert (string_of_expr defn_func = "(int my_func = my_var = \"my_val\") {\n\"hello world\";\n}")


(* Test cases for Call *)
let run_expr_tests_func_invoc_cases () =
  let basic_invocation = Call(Function([Decl(Int, "my_first_func_invoc")], [Expr(StrLit("hello world"))]), [Literal 1; Literal 2; Literal 3]) in
  let multiple_arg_invocation = Call(Function([Decl(Int, "multiple_arg_invoc")], [Expr(StrLit("hello world"))]), [Literal 1; BoolLit(true); ChrLit('c')]) in
  assert (string_of_expr basic_invocation = "(int my_first_func_invoc) {\n\"hello world\";\n}(1)(2)(3)");
  assert (string_of_expr multiple_arg_invocation = "(int multiple_arg_invoc) {\n\"hello world\";\n}(1)(true)(c)")

let run_type_tests () =
  let typ1 = Int in
  let typ2 = Bool in
  let typ3 = Char in
  let typ4 = String in
  let typ5 = Void in
  let typ6 = Float in
  let typ7 = List Int in
  let typ8 = List Bool in
  let typ9 = List Char in
  let typ10 = List String in
  let typ11 = List Void in
  let typ12 = List Float in
  assert (string_of_typ typ1 = "int");
  assert (string_of_typ typ2 = "bool");
  assert (string_of_typ typ3 = "char");
  assert (string_of_typ typ4 = "string");
  assert (string_of_typ typ5 = "void");
  assert (string_of_typ typ6 = "float");
  assert (string_of_typ typ7 = "list<int>");
  assert (string_of_typ typ8 = "list<bool>");
  assert (string_of_typ typ9 = "list<char>");
  assert (string_of_typ typ10 = "list<string>");
  assert (string_of_typ typ11 = "list<void>");
  assert (string_of_typ typ12 = "list<float>")

let run_type_tests_struct_cases () =
  let simple_struct_sig = StructSig("simple_struct_sig") in
  let simple_struct_mem = StructMem("my_first_signature", [Decl(Int, "declaration_int")]) in
  let complex_struct_mem = StructMem("complex_signature", [Decl(Int, "declaration_int"); Defn(String, "string_declr", StrLit("my_val")); Defn(Bool, "boolean_d", BoolLit(false))]) in
  assert (string_of_typ simple_struct_sig = "simple_struct_sig");
  assert (string_of_typ simple_struct_mem = "my_first_signature int declaration_int");
  assert (string_of_typ complex_struct_mem = "complex_signature int declaration_int,string string_declr = \"my_val\",bool boolean_d = false")


let run_type_tests_fun_sig_cases () =
  let simple_func_sig = FunSig([Int], Int) in
  let complex_func_sig = FunSig([Int; Bool; Char; String; Void; Float], List Int) in
  let list_func_sig = FunSig([List Int; List Char], Void) in
  assert (string_of_typ simple_func_sig = "function<int> -> int");
  assert (string_of_typ complex_func_sig = "function<int, bool, char, string, void, float> -> list<int>");
  assert (string_of_typ list_func_sig = "function<list<int>, list<char>> -> void")

let run_stmt_tests () =
  let stmt1 = Block([Expr(Zero); Expr(Zero); Expr(Zero)]) in
  let stmt2 = Expr(Zero) in
  let stmt3 = Bind(Decl(Int, "hello_world_func")) in
  let stmt4 = IfElse(BoolLit(true), Expr(StrLit("its true")), Expr(StrLit("its false"))) in
  let stmt5 = While(BoolLit(true), Expr(StrLit("its true"))) in
  let stmt6 = For(Decl(Int, "hello_world_func"), StrLit("its true"), StrLit("its false"), Expr(Zero)) in
  let stmt7 = If(BoolLit(true), Expr(StrLit("its true"))) in
  assert (string_of_stmt stmt1 = "{\n0;\n0;\n0;\n}\n");
  assert (string_of_stmt stmt2 = "0;\n");
  assert (string_of_stmt stmt3 = "int hello_world_func;\n");
  assert (string_of_stmt stmt4 = "if (true)\n\"its true\";\nelse\n\"its false\";\n");
  assert (string_of_stmt stmt5 = "while (true) \"its true\";\n");
  assert (string_of_stmt stmt6 = "for (int hello_world_func; \"its true\"; \"its false\") {\n0;\n}");
  assert (string_of_stmt stmt7 = "if (true)\n\"its true\";\n")


(* Runs all tests... look at each function for the actual cases *)
let run_tests () =
  run_op_tests ();
  run_expr_tests ();
  run_expr_tests_edge_cases ();
  run_expr_tests_function_and_bind_cases ();
  run_expr_tests_func_invoc_cases ();
  run_type_tests ();
  run_type_tests_struct_cases ();
  run_type_tests_fun_sig_cases ();
  run_stmt_tests ()

(* Execute test suite for ast.ml *)
let () =
  run_tests ();
  print_endline "test_ast.ml passed"
