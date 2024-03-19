/* Ocamlyacc parser for FunC */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE ASSIGN
%token PLUS MINUS DIVIDE TIMES MOD
%token PPLUS MMINUS
%token PLUSEQ MINUSEQ
%token LBRACK RBRACK LT GT LEQ GEQ
%token EQ NEQ AND OR NOT
%token IF ELSE WHILE FOR
%token INT BOOL FLOAT CHAR VOID STRING
%token FUNC OUTPUT LIST STRUCT LAMBDA FUNCARROW
%token RETURN COMMA
%token <int> LITERAL
%token <string> STRING_LIT
%token <float> FLOAT_LIT
%token <char> CHAR_LIT
%token <bool> BLIT
%token <string> ID
%token EOF

%start program_rule
%type <Ast.program> program_rule

%right ASSIGN STRUCT
%left OR
%left AND
%left EQ NEQ
%left LT GT
%left PLUS MINUS
%left TIMES DIVIDE

%%

program_rule:
  stmt_list_rule EOF              { {body=$1} }

typ_list_rule:
  /* nothing */                   { [] }
  // | typ_rule COMMA typ_rule       { $1 :: $3 }
  | typ_rule COMMA typ_list_rule  { $1 :: $3 }

bind_list_rule:
  /* nothing */                 { [] }
  | bind_rule bind_list_rule    { $1 :: $2 }

stmt_list_rule:
    /* nothing */               { []     }
    | stmt_rule stmt_list_rule  { $1::$2 }

expr_list_rule:
  /* nothing */                       { [] }
  | expr_rule COMMA expr_list_rule    { $1 :: $3 }

typ_rule:
  INT                                         { Int }
  | BOOL                                      { Bool }
  | CHAR                                      { Char }
  | STRING                                    { String }
  | FLOAT                                     { Float }
  | VOID                                      { Void }
  | LIST typ_rule                             { List $2 }
  | STRUCT ID LBRACE bind_list_rule RBRACE    { StructSig($2, $4) }
  | FUNC LT typ_list_rule GT OUTPUT typ_rule  { FunSig($3, $6) }

bind_rule:
  typ_rule ID ASSIGN expr_rule   { Defn($1, $2, $4) }
  | typ_rule ID                  { Decl($1, $2) }

stmt_rule:
  | expr_rule SEMI                                                      { Expr $1                }
  | bind_rule SEMI                                                      { Bind $1                }
  | LBRACE stmt_list_rule RBRACE                                        { Block $2               }
  | IF LPAREN expr_rule RPAREN stmt_rule ELSE stmt_rule                 { If ($3, $5, $7)        }
  | WHILE LPAREN expr_rule RPAREN stmt_rule                             { While ($3, $5)         }
  | FOR LPAREN bind_rule SEMI expr_rule SEMI expr_rule RPAREN stmt_rule { For ($3, $5, $7, $9)   }

expr_rule:
  | BLIT                          { BoolLit $1            }
  | CHAR_LIT                      { ChrLit $1             }
  | STRING_LIT                    { StrLit $1             }
  | FLOAT_LIT                     { FloatLit $1 }
  | LITERAL                       { Literal $1 }
  | LBRACK expr_list_rule RBRACK  { ListLit($2) }
  | ID                            { Id $1 }
  | expr_rule PLUS expr_rule      { Binop ($1, Add, $3) }
  | expr_rule MINUS expr_rule     { Binop ($1, Sub, $3) }
  | expr_rule TIMES expr_rule     { Binop ($1, Mult, $3) }
  | expr_rule DIVIDE expr_rule    { Binop ($1, Div, $3) }
  | expr_rule EQ expr_rule        { Binop ($1, Equal, $3) }
  | expr_rule NEQ expr_rule       { Binop ($1, Neq, $3) }
  | expr_rule LT expr_rule        { Binop ($1, Less, $3) }
  | expr_rule GT expr_rule        { Binop ($1, Greater, $3) }
  | expr_rule AND expr_rule       { Binop ($1, And, $3) }
  | expr_rule OR expr_rule        { Binop ($1, Or, $3) }
  | PLUS expr_rule                { $2 }
  | MINUS expr_rule               { Binop(Zero, Sub, $2) }
  | ID ASSIGN expr_rule           { Assign ($1, $3) }
  | LPAREN bind_list_rule RPAREN FUNCARROW stmt_list_rule { Function($2, $5) }        
  | LPAREN expr_rule RPAREN       { $2 }
  | LBRACE expr_list_rule RBRACE SEMI  { Struct($2) }