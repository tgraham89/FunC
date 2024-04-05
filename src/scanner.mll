(* Ocamllex scanner for FunC *)

{ open Parser }

let alpha = ['a' - 'z' 'A' - 'Z']
let digit = ['0'-'9']
let id = alpha (alpha | digit | '_')*
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let escape_char = ''' (escape) '''
let char = ''' ( ascii | digit ) '''
let string = ''' (ascii | escape_char )* '''
let float = (digit)*['.']digit(digit)+
let intlit = (['1'-'9']['0'-'9']* | '0')
let stringlit = '"'((ascii|escape)* as lxm)'"'


rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '|'      { VBAR }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACK }
| ']'      { RBRACK }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '/'      { DIVIDE }
| '*'      { TIMES }
| '%'      { MOD }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| '>'      { GT }
| "<="     { LEQ }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| '!'      { NOT }
| "++"     { PPLUS }
| "--"     { MMINUS }
| "+="     { PLUSEQ }
| "-="     { MINUSEQ }
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
| "for"    { FOR }
| "return" { RETURN }
| "int"    { INT }
| "float"  { FLOAT }
| "char"   { CHAR }
| "bool"   { BOOL }
| "string" { STRING }
| "void"   { VOID }
| "lambda" { LAMBDA }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| "function" { FUNC }
| "->"       { OUTPUT }
| ":"       { COLON }
| "=>"     { FUNCARROW }
| "list"    { LIST }
| "struct"  { STRUCT }
| intlit as lxm { LITERAL(int_of_string lxm) }
| float as lxm { FLOAT_LIT(float_of_string lxm) }
| char as lxm { CHAR_LIT( String.get lxm 1 ) }
(* | escape_char as lxm{ CHAR_LIT( String.get (unescape lxm) 1) } Not sure if this is necessary *) 
| stringlit { STRING_LIT(lxm) }
| id as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }