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
  | PLUSEQ -> "MMINUS"
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



(* Series of basic tests for simple string statements *) 
let run_basic_tests () =
  let string1 = "int x = 123;" in
  let actual_tokens = string_of_tokens string1 in
  assert (actual_tokens = ["INT"; "ID"; "ASSIGN"; "LITERAL"; "SEMI"; "EOF"])
  (* TODO: add for rest of primitives *)
  

(* Runs all tests *) 
let run_tests () =
  run_basic_tests ()
  (* TODO: add tests for every token *)

(* Execute test suite for scanner file *)
let () =
  run_tests ();
  print_endline "test_scanner.ml passed"

