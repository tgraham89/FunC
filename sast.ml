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
  | SIf of sexpr * sstmt
  | SIfElse of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | SFor of sbind * sexpr * sexpr * sstmt
and program = {
  sbody: sstmt list;
}


(* Pretty-printing functions *)

let rec string_of_sexpr (t, e) = 
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SZero -> "0"
    | SLiteral(l) -> string_of_int l
    | SBoolLit(true) -> "true"
    | SBoolLit(false) -> "false"
    | SStrLit(s) -> "\"" ^ s ^ "\""
    | SChrLit(c) -> String.make 1 c
    | SFloatLit(f) -> string_of_float f
    (* | SListLit(lst) -> "[" ^ string_of_sexpr_list ", " lst ^ "]"  *)
    (* | SStruct(lst) -> "{" ^ string_of_sexpr_list ", " lst ^ "}"  *)
    | SId(s) -> s
    | SBinop(e1, o, e2) -> string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
    | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
    (* | SFunction(args, body) -> "(" ^ string_of_sbind_list ", " args ^ ") {\n\t" ^ string_of_sstmt_list ";\n\t" body ^ "}" *)
    (* | SFuncInvoc(id, args) -> id ^ "(" ^ string_of_sexpr_list ", " args ^ ")" *)
    | _ -> "string_of_sexpr not implemented yet"
    ) ^ ")"

  (* todo need to add the other sexpr's *)

  and string_of_sbind = function
  | SDecl(t, id) -> string_of_typ t ^ " " ^ id
  | SDefn(t, id, e) -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_sexpr e

  and string_of_sstmt = function
  | SBlock(stmts) -> "{\n" ^ String.concat "\n" (List.map string_of_sstmt stmts) ^ "\n}"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SBind(bind) -> string_of_sbind bind ^ ";\n"
  | SIf(cond, branch) -> "if (" ^ string_of_sexpr cond ^ ")\n" ^ string_of_sstmt branch
  | SIfElse(cond, then_stmt, else_stmt) -> "if (" ^ string_of_sexpr cond ^ ")\n" ^ string_of_sstmt then_stmt ^ "else\n" ^ string_of_sstmt else_stmt
  | SWhile(cond, stmt) -> "while (" ^ string_of_sexpr cond ^ ")\n" ^ string_of_sstmt stmt
  | SFor(init, cond, incr, stmt) -> "for (" ^ string_of_sbind init ^ "; " ^ string_of_sexpr cond ^ "; " ^ string_of_sexpr incr ^ ")\n" ^ string_of_sstmt stmt

  let string_of_sprogram fdecl =
    "\n\nSementically checked program: \n\n" ^
    String.concat "\n" (List.map string_of_sstmt fdecl.sbody) ^
    "\n"
