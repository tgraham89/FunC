open Parser

(* Converts tokens into a string representation *)
let string_of_token = function
  INT -> "INT"
  | BOOL -> "BOOL"
  | FLOAT -> "FLOAT"
  | CHAR -> "CHAR"
  | VOID -> "VOID"
  | STRING -> "STRING"
  | SEMI -> "SEMI"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | ASSIGN -> "ASSIGN"
  | PPLUS -> "PPLUS"
  | MMINUS -> "MMINUS"
  | PLUSEQ -> "PLUSEQ"
  | MINUSEQ -> "MINUSEQ"
  | LBRACK -> "LBRACK"
  | RBRACK -> "RBRACK"
  | LT -> "LT"
  | GT -> "GT"
  | LEQ -> "LEQ"
  | GEQ -> "GEQ"
  | VBAR -> "VBAR"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | COLON -> "COLON"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | WHILE -> "WHILE"
  | FOR -> "FOR"
  | FUNC -> "FUNC"
  | OUTPUT -> "OUTPUT"
  | LIST -> "LIST"
  | STRUCT -> "STRUCT"
  | LAMBDA -> "LAMBDA"
  | FUNCARROW -> "FUNCARROW"
  | RETURN -> "RETURN"
  | COMMA -> "COMMA"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | DIVIDE -> "DIVIDE"
  | TIMES -> "TIMES"
  | MOD -> "MOD"
  | LITERAL l -> "LITERAL"
  | STRING_LIT sl -> "STRING_LIT"
  | FLOAT_LIT fl -> "FLOAT_LIT"
  | CHAR_LIT cl -> "CHAR_LIT"
  | BLIT bl -> "BLIT"
  | ID i -> "ID"
  | _ -> raise (Failure "Undeclared token")


(* Convert a string into tokens via the scanner. Tokens defined by the parser *)
let string_of_tokens s =
  let lexbuf = Lexing.from_string s in
  let rec process_token lexbuf =
    let t = Scanner.token lexbuf in
    match t with
      EOF -> ["EOF"]
      | _ ->
        let str_t = string_of_token t in
        str_t :: process_token lexbuf
    in
  process_token lexbuf


(* Dev friendly helper func to print out tokens for debugging purposes *)
let rec print_tokens ts =
  match ts with
  | t :: ts ->
    print_endline t;
    print_tokens ts
  | [] -> ()


(* Helper funcs ^^^^^ *)

(* Test funcs vvvvvv *)



(* Series of tests for primitives *) 
let run_primitive_tests () =
  let test1 = "int x = 123;" in
  let test2 = "string x = \"hello world\";" in
  let test3 = "bool x = true;" in
  let test4 = "float x = 1.23;" in
  let test5 = "char x = \"a\";" in
  let test6 = "void x;" in
  let test7 = "bool x = false;" in
  let actual_tokens1 = string_of_tokens test1 in
  let actual_tokens2 = string_of_tokens test2 in
  let actual_tokens3 = string_of_tokens test3 in
  let actual_tokens4 = string_of_tokens test4 in
  let actual_tokens5 = string_of_tokens test5 in
  let actual_tokens6 = string_of_tokens test6 in
  let actual_tokens7 = string_of_tokens test7 in
  assert (actual_tokens1 = ["INT"; "ID"; "ASSIGN"; "LITERAL"; "SEMI"; "EOF"]);
  assert (actual_tokens2 = ["STRING"; "ID"; "ASSIGN"; "STRING_LIT"; "SEMI"; "EOF"]);
  assert (actual_tokens3 = ["BOOL"; "ID"; "ASSIGN"; "BLIT"; "SEMI"; "EOF"]);
  assert (actual_tokens4 = ["FLOAT"; "ID"; "ASSIGN"; "FLOAT_LIT"; "SEMI"; "EOF"]);
  assert (actual_tokens5 = ["CHAR"; "ID"; "ASSIGN"; "STRING_LIT"; "SEMI"; "EOF"]);
  assert (actual_tokens6 = ["VOID"; "ID"; "SEMI"; "EOF"]);
  assert (actual_tokens7 = ["BOOL"; "ID"; "ASSIGN"; "BLIT"; "SEMI"; "EOF"])

(* Series of tests for operators *) 
let run_operators_tests () =
  let test1 = "bool x = 1 > 2;" in
  let test2 = "bool x = 1 < 2;" in
  let test3 = "bool x = 1 >= 2;" in
  let test4 = "bool x = 1 <= 2;" in
  let test5 = "bool x = 1 == 2;" in
  let test6 = "bool x = 1 != 2;" in
  let test7 = "bool x = true && false;" in
  let test8 = "bool x = true || false;" in
  let test9 = "bool x = !false;" in
  let test10 = "int x = 1++;" in
  let test11 = "int x = 1--;" in
  let test12 = "int x = 1; x += 1;" in
  let test13 = "int x = 1; x -= 1;" in
  let test14 = "int x = 1 + 1;" in
  let test15 = "int x = 1 - 1;" in
  let test16 = "int x = 1 * 1;" in
  let test17 = "int x = 1 / 1;" in
  let test18 = "int x = 1 % 1;" in
  let test19 = "bool x = 0 | 1;" in
  let actual_tokens1 = string_of_tokens test1 in
  let actual_tokens2 = string_of_tokens test2 in
  let actual_tokens3 = string_of_tokens test3 in
  let actual_tokens4 = string_of_tokens test4 in
  let actual_tokens5 = string_of_tokens test5 in
  let actual_tokens6 = string_of_tokens test6 in
  let actual_tokens7 = string_of_tokens test7 in
  let actual_tokens8 = string_of_tokens test8 in
  let actual_tokens9 = string_of_tokens test9 in
  let actual_tokens10 = string_of_tokens test10 in
  let actual_tokens11 = string_of_tokens test11 in
  let actual_tokens12 = string_of_tokens test12 in
  let actual_tokens13 = string_of_tokens test13 in
  let actual_tokens14 = string_of_tokens test14 in
  let actual_tokens15 = string_of_tokens test15 in
  let actual_tokens16 = string_of_tokens test16 in
  let actual_tokens17 = string_of_tokens test17 in
  let actual_tokens18 = string_of_tokens test18 in
  let actual_tokens19 = string_of_tokens test19 in
  assert (actual_tokens1 = ["BOOL"; "ID"; "ASSIGN"; "LITERAL"; "GT"; "LITERAL"; "SEMI"; "EOF"]);
  assert (actual_tokens2 = ["BOOL"; "ID"; "ASSIGN"; "LITERAL"; "LT"; "LITERAL"; "SEMI"; "EOF"]);
  assert (actual_tokens3 = ["BOOL"; "ID"; "ASSIGN"; "LITERAL"; "GEQ"; "LITERAL"; "SEMI"; "EOF"]);
  assert (actual_tokens4 = ["BOOL"; "ID"; "ASSIGN"; "LITERAL"; "LEQ"; "LITERAL"; "SEMI"; "EOF"]);
  assert (actual_tokens5 = ["BOOL"; "ID"; "ASSIGN"; "LITERAL"; "EQ"; "LITERAL"; "SEMI"; "EOF"]);
  assert (actual_tokens6 = ["BOOL"; "ID"; "ASSIGN"; "LITERAL"; "NEQ"; "LITERAL"; "SEMI"; "EOF"]);
  assert (actual_tokens7 = ["BOOL"; "ID"; "ASSIGN"; "BLIT"; "AND"; "BLIT"; "SEMI"; "EOF"]);
  assert (actual_tokens8 = ["BOOL"; "ID"; "ASSIGN"; "BLIT"; "OR"; "BLIT"; "SEMI"; "EOF"]);
  assert (actual_tokens9 = ["BOOL"; "ID"; "ASSIGN"; "NOT"; "BLIT"; "SEMI"; "EOF"]);
  assert (actual_tokens10 = ["INT"; "ID"; "ASSIGN"; "LITERAL"; "PPLUS"; "SEMI"; "EOF"]);
  assert (actual_tokens11 = ["INT"; "ID"; "ASSIGN"; "LITERAL"; "MMINUS"; "SEMI"; "EOF"]);
  assert (actual_tokens12 = ["INT"; "ID"; "ASSIGN"; "LITERAL"; "SEMI"; "ID"; "PLUSEQ"; "LITERAL"; "SEMI"; "EOF"]);
  assert (actual_tokens13 = ["INT"; "ID"; "ASSIGN"; "LITERAL"; "SEMI"; "ID"; "MINUSEQ"; "LITERAL"; "SEMI"; "EOF"]);
  assert (actual_tokens14 = ["INT"; "ID"; "ASSIGN"; "LITERAL"; "PLUS"; "LITERAL"; "SEMI"; "EOF"]);
  assert (actual_tokens15 = ["INT"; "ID"; "ASSIGN"; "LITERAL"; "MINUS"; "LITERAL"; "SEMI"; "EOF"]);
  assert (actual_tokens16 = ["INT"; "ID"; "ASSIGN"; "LITERAL"; "TIMES"; "LITERAL"; "SEMI"; "EOF"]);
  assert (actual_tokens17 = ["INT"; "ID"; "ASSIGN"; "LITERAL"; "DIVIDE"; "LITERAL"; "SEMI"; "EOF"]);
  assert (actual_tokens18 = ["INT"; "ID"; "ASSIGN"; "LITERAL"; "MOD"; "LITERAL"; "SEMI"; "EOF"]);
  assert (actual_tokens19 = ["BOOL"; "ID"; "ASSIGN"; "LITERAL"; "VBAR"; "LITERAL"; "SEMI"; "EOF"])

(* Series of tests for statements like if/else, loops, etc *) 
let run_statement_tests () = 
  let test1 = "for (int x = 0; x < 5; x = x + 1) { 5; }" in
  let test2 = "function<> -> int main = () => { return 0; };" in
  let test3 = "if (1 < 2) { return 0; };" in
  let test4 = "struct x { string name, int id, };" in
  let test5 = "lambda -> int main = () => { return 0; };" in
  let actual_tokens1 = string_of_tokens test1 in
  let actual_tokens2 = string_of_tokens test2 in
  let actual_tokens3 = string_of_tokens test3 in
  let actual_tokens4 = string_of_tokens test4 in
  let actual_tokens5 = string_of_tokens test5 in
  assert (actual_tokens1 = ["FOR"; "LPAREN"; "INT"; "ID"; "ASSIGN"; "LITERAL"; "SEMI"; "ID"; "LT"; "LITERAL"; "SEMI";
    "ID"; "ASSIGN"; "ID"; "PLUS"; "LITERAL"; "RPAREN"; "LBRACE"; "LITERAL"; "SEMI"; "RBRACE"; "EOF"]);
  assert (actual_tokens2 = ["FUNC"; "LT"; "GT"; "OUTPUT"; "INT"; "ID"; "ASSIGN"; "LPAREN"; "RPAREN"; "FUNCARROW";
    "LBRACE"; "RETURN"; "LITERAL"; "SEMI"; "RBRACE"; "SEMI"; "EOF"]);
  assert (actual_tokens3 = ["IF"; "LPAREN"; "LITERAL"; "LT"; "LITERAL"; "RPAREN"; "LBRACE"; "RETURN";
    "LITERAL"; "SEMI"; "RBRACE"; "SEMI"; "EOF";]);
  assert (actual_tokens4 = ["STRUCT"; "ID"; "LBRACE"; "STRING"; "ID"; "COMMA"; "INT"; "ID"; "COMMA";
    "RBRACE"; "SEMI"; "EOF"]);
  assert (actual_tokens5 = ["LAMBDA"; "OUTPUT"; "INT"; "ID"; "ASSIGN"; "LPAREN"; "RPAREN"; "FUNCARROW";
    "LBRACE"; "RETURN"; "LITERAL"; "SEMI"; "RBRACE"; "SEMI"; "EOF"])


(* Runs all tests *) 
let run_tests () =
  run_primitive_tests ();
  run_operators_tests ();
  run_statement_tests ()

(* Execute test suite for scanner file *)
let () =
  run_tests ();
  print_endline "unit_tests_scanner.ml passed"


