/* Ocamlyacc parser for FunC */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE ASSIGN
%token PLUS MINUS DIVIDE TIMES MOD
%token PPLUS MMINUS
%token PLUSEQ MINUSEQ
%token LBRACK RBRACK LT GT LEQ GEQ VBAR
%token DOT
%token EQ NEQ AND OR NOT COLON
%token IF ELSE WHILE FOR
%token INT BOOL FLOAT CHAR VOID STRING STRUCT
%token FUNC OUTPUT LIST LAMBDA FUNCARROW
%token RETURN COMMA
%token <int> LITERAL
%token <string> STRING_LIT
%token <float> FLOAT_LIT
%token <char> CHAR_LIT
%token <bool> BLIT
%token <string> ID
%token <string> STRUCT_ID
%token EOF
// %token PLUS MINUS LITERAL
// %right PLUS MINUS


%start program_rule
%type <Ast.program> program_rule

%right ASSIGN
%left OR
%left AND
%left NOT
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD

%%

program_rule:
  stmt_list_rule EOF              { {body=$1} }

typ_list_rule:
  /* nothing */                   { [] } 
  | typ_rule                      { [$1] }
  | typ_rule COMMA typ_list_rule  { $1 :: $3 }

bind_list_rule:
  /* nothing */                       { [] }
  | bind_rule                         { [$1] }
  | bind_rule COMMA bind_list_rule    { $1 :: $3 }

stmt_list_rule:
    // /* nothing */                       { []  }
    | stmt_rule                         { [$1]     }
    | stmt_list_rule stmt_rule          { $1 @ [$2] }

expr_list_rule:
  /* nothing */                         { [] }
  | expr_rule                           { [$1] }
  | expr_rule COMMA expr_list_rule      { $1 :: $3 }

typ_rule:
  INT                                         { Int }
  | BOOL                                      { Bool }
  | CHAR                                      { Char }
  | STRING                                    { String }
  | FLOAT                                     { Float }
  | VOID                                      { Void }
  | LIST LT typ_rule GT                       { List $3 }
  | STRUCT_ID                                    { StructSig($1) }
  | FUNC LT typ_list_rule GT OUTPUT typ_rule  { FunSig($3, $6) }
  | LAMBDA LT typ_list_rule GT OUTPUT typ_rule { FunSig($3, $6) }

bind_rule:
  typ_rule ID ASSIGN expr_rule   { Defn($1, $2, $4) }
  | typ_rule ID                  { Decl($1, $2) }
  // | typ_rule STRUCT_ID                  { Decl($1, $2) }
  | typ_rule COLON expr_rule     { Defn($1, "anon", $3) }

stmt_rule:
  open_stmt                                                             { $1 }
  | closed_stmt                                                         { $1 }

open_stmt:
  IF LPAREN expr_rule RPAREN stmt_rule                                  { If($3, $5) }
  | IF LPAREN expr_rule RPAREN closed_stmt ELSE open_stmt                 { IfElse($3, $5, $7) }

closed_stmt:
   IF LPAREN expr_rule RPAREN closed_stmt ELSE closed_stmt               { IfElse ($3, $5, $7)    }
  | WHILE LPAREN expr_rule RPAREN closed_stmt                             { While ($3, $5)         }
  | FOR LPAREN bind_rule SEMI expr_rule SEMI expr_rule RPAREN closed_stmt { For ($3, $5, $7, $9)   }
  | STRUCT STRUCT_ID LBRACE struct_member_rule RBRACE SEMI                   { StructDecl {sname = $2; members = $4}}
  // | STRUCT_ID ID LBRACE struct_member_assign_rule RBRACE                    { Struct_decl ($2, $4)}
  | simple_stmt                                                             { $1 }

struct_member_rule:
  struct_member                                                             { [$1] }
  | struct_member_rule struct_member                                        {$1 @ [$2]}

struct_member:
  typ_rule ID COMMA                                                       {Decl($1, $2)}

struct_member_assign_rule:
  struct_member_assign                                                      {[$1]}
  | struct_member_assign_rule struct_member_assign                          {$1 @ [$2]}

struct_member_assign:
   ID ASSIGN expr_rule COMMA                                    { Assign($1, $3) }

simple_stmt:
  expr_rule SEMI                                                          { Expr $1                }
  | bind_rule SEMI                                                        { Bind $1                }
  | LBRACE stmt_list_rule RBRACE                                          { Block $2               }
  | RETURN expr_rule SEMI                                                 { Return $2              }


func_rule:
  LPAREN bind_list_rule RPAREN FUNCARROW LBRACE stmt_list_rule RBRACE { Function($2, $6) }        


expr_rule:
  | BLIT                          { BoolLit $1            }
  | CHAR_LIT                      { ChrLit $1             }
  | STRING_LIT                    { StrLit $1             }
  | FLOAT_LIT                     { FloatLit $1 }
  | LITERAL                       { Literal $1 }
  | STRUCT_ID                     { StructId $1 }
  | LBRACK expr_list_rule RBRACK  { ListLit($2) }
  | ID                            { Id $1 }
  | PLUS expr_rule  { UnaryOp (Pos, $2) }
  | MINUS expr_rule { UnaryOp (Neg, $2) }
  // | PLUS LITERAL                  { Literal $2 }
  // | MINUS expr_rule                 { Binop(0, Sub, $2) }
  | expr_rule PLUS expr_rule      { Binop ($1, Add, $3) }
  | expr_rule MINUS expr_rule     { Binop ($1, Sub, $3) }
  | expr_rule TIMES expr_rule     { Binop ($1, Mult, $3) }
  | expr_rule DIVIDE expr_rule    { Binop ($1, Div, $3) }
  | expr_rule MOD expr_rule       { Binop ($1, Mod, $3) }
  | expr_rule EQ expr_rule        { Binop ($1, Equal, $3) }
  | expr_rule NEQ expr_rule       { Binop ($1, Neq, $3) }
  | expr_rule LT expr_rule        { Binop ($1, Less, $3) }
  | expr_rule LEQ expr_rule       { Binop ($1, Lequal, $3) }
  | expr_rule GT expr_rule        { Binop ($1, Greater, $3) }
  | expr_rule GEQ expr_rule       { Binop ($1, Gequal, $3) }
  | expr_rule AND expr_rule       { Binop ($1, And, $3) }
  | expr_rule OR expr_rule        { Binop ($1, Or, $3) }
  | expr_rule NOT expr_rule       { Binop ($1, Not, $3) }
  | ID ASSIGN expr_rule           { Assign ($1, $3) }
  | ID LPAREN expr_list_rule RPAREN      { Call((Id $1), $3) }
  | func_rule                                   { $1 }
  | func_rule LPAREN expr_list_rule RPAREN      { Call($1, $3) }        
  | LPAREN expr_rule RPAREN       { $2 }
  | LBRACE struct_member_assign_rule RBRACE { StructAssign($2)}
  // | STRUCT STRUCT_ID LBRACE stmt_list_rule RBRACE { StructCreate($2, $4)}
  // | ID DOT ID                     { StructAccess (StructId($1), Id($3))}
  | ID DOT ID                     { StructAccess (StructId($1 ^ "." ^ $3))}
  | ID DOT ID ASSIGN expr_rule {Assign($1 ^ "." ^ $3, $5)}
