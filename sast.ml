open Ast

type sexpr = typ * sx
and sx =
  | SLiteral of int
  | SBoolLit of bool
  | SStrLit of string
  | SChrLit of char
  | SFloatLit of float
  | SId of string
  | SBinop of sexpr * bop * sexpr
  | SAssign of string * sexpr
  | SListLit of sexpr list
  | SStruct of sexpr list
  | SFunction of sbind list * sstmt list
  | SFuncInvoc of string * sexpr list
  | SZero
and sbind =
  | SDecl of typ * string
  | SDefn of typ * string * sexpr
and sstmt =
  | SBlock of sstmt list
  | SExpr of sexpr
  | SBind of sbind
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | SFor of sbind * sexpr * sexpr * sstmt




(* Pretty-printing functions *)

let rec string_of_sexpr = function
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SStrLit(s1) -> s1
  | SChrLit(c1) -> c1
  | SFloatLit(f1) -> string_of_float f1
  | SId(i1) -> i1
  | SBinop(l1, o1, r1) -> string_of_sexpr l1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr r1
  | SAssign(sa1, sa2) -> sa1 ^ "=" ^ string_of_sexpr sa2
  (* | SListLit() ->  "listlit"
  | SStruct -> "struct"
  | SFunction -> "func"
  | SFuncInvoc -> "funcInvoc" *)
  | SZero -> "0"
  | _ -> "string_of_sexpr not implemented yet"

  (* todo need to add the other sexpr's *)

  and string_of_sbind = function
  | SDecl(t, id) -> string_of_typ t ^ " " ^ id
  | SDefn(t, id, e) -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_sexpr e

  and string_of_sstmt = function
  | SBlock(stmts) -> "{\n" ^ String.concat "\n" (List.map string_of_sstmt stmts) ^ "\n}"
  | SExpr(expr) -> string_of_sexpr expr ^ ";"
  | SBind(bind) -> string_of_sbind bind ^ ";"
  | SIf(cond, then_stmt, else_stmt) -> "if (" ^ string_of_sexpr cond ^ ")\n" ^ string_of_sstmt then_stmt ^ "else\n" ^ string_of_sstmt else_stmt
  | SWhile(cond, stmt) -> "while (" ^ string_of_sexpr cond ^ ")\n" ^ string_of_sstmt stmt
  | SFor(init, cond, incr, stmt) -> "for (" ^ string_of_sbind init ^ "; " ^ string_of_sexpr cond ^ "; " ^ string_of_sexpr incr ^ ")\n" ^ string_of_sstmt stmt

let string_of_sprogram prog =
  String.concat "\n" (List.map string_of_sstmt prog.sbody)