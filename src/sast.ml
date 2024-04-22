open Ast

type sunary_operator = SNeg | SPos

type sexpr = typ * sx
and sx =
  | SUnaryOp of sunary_operator * sexpr
  | SLiteral of int
  | SBoolLit of bool
  | SStrLit of string
  | SChrLit of char
  | SFloatLit of float
  | SId of string
  | SBinop of sexpr * bop * sexpr
  | SAssign of string * sexpr
  | SListLit of typ * sexpr list
  | SFunction of sbind list * sstmt list
  | SCall of sexpr * sexpr list
  | SStructId of string
  | SStructAccess of sexpr
  | SStructAssign of sexpr list (* Used to define an instance of a struct *)
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
  | SReturn of sexpr
  | SStructDecl of {
        sname: string;
        members: sbind list;
        }
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
    | SListLit(ty, lst) -> "( list of " ^ string_of_typ ty ^ ": [" ^ string_of_sexpr_list ", " lst ^ "]" 
    (* | SStruct(lst) -> "{" ^ string_of_sexpr_list ", " lst ^ "}"  *)
    | SId(s) -> s
    | SStructId(s) -> s
    | SBinop(e1, o, e2) -> string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
    | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
    | SFunction(args, body) -> "(" ^ string_of_sbind_list ", " args ^ ") {\n\t" ^ string_of_sstmt_list "\t" body ^ "\n}"
    | SCall(sx, args) -> string_of_sexpr sx ^ 
                        "(" ^ string_of_sexpr_list ", " args ^ ")"
    (* | SStructAccess(str, mem) -> "(" ^ str ^ "." ^ mem ^ ")" *)
    (* | SStructAccess(str, mem) -> "(" ^ string_of_sexpr str ^ "." ^ string_of_sexpr mem ^ ")" *)
    | SStructAccess(str) -> string_of_sexpr str
    | SStructAssign(e) -> "(" ^ string_of_sexpr_list ", " e ^ ")"
    | SUnaryOp(op, ex) -> match op with SNeg -> "-" ^ string_of_sexpr ex ^ ""
                                    | SPos -> "+" ^  string_of_sexpr ex ^ ""
    ) ^ ")"

  (* todo need to add the other sexpr's *)
  and string_of_sexpr_list delim = function
  [] -> "" | x :: [] -> string_of_sexpr x | x :: rest -> string_of_sexpr x ^ delim ^ string_of_sexpr_list delim rest
  and string_of_sbind = function
  | SDecl(t, id) -> string_of_typ t ^ " " ^ id
  | SDefn(t, id, e) -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_sexpr e
  and string_of_sbind_list delim = function
  [] -> ""
  | x :: [] -> string_of_sbind x
  | x :: rest -> string_of_sbind x ^ delim ^ string_of_sbind_list delim rest

  and string_of_sstmt = function
  | SBlock(stmts) -> "{\n" ^ String.concat "\n" (List.map string_of_sstmt stmts) ^ "\n}"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SBind(bind) -> string_of_sbind bind ^ ";\n"
  | SIf(cond, branch) -> "if (" ^ string_of_sexpr cond ^ ")\n" ^ string_of_sstmt branch
  | SIfElse(cond, then_stmt, else_stmt) -> "if (" ^ string_of_sexpr cond ^ ")\n" ^ string_of_sstmt then_stmt ^ "else\n" ^ string_of_sstmt else_stmt
  | SWhile(cond, stmt) -> "while (" ^ string_of_sexpr cond ^ ")\n" ^ string_of_sstmt stmt
  | SFor(init, cond, incr, stmt) -> "for (" ^ string_of_sbind init ^ "; " ^ string_of_sexpr cond ^ "; " ^ string_of_sexpr incr ^ ")\n" ^ string_of_sstmt stmt
  | SReturn(value) -> "return " ^ string_of_sexpr value ^ ";"
  | SStructDecl(s) -> "struct " ^ s.sname ^ " {\n" ^ string_of_sbind_list ",\n" s.members ^ ",\n};\n"

  and string_of_sstmt_list delim = function
  [] -> ""
  | x :: [] -> string_of_sstmt x ^ delim
  | x :: rest -> string_of_sstmt x ^ delim ^ string_of_sstmt_list delim rest

  let string_of_sprogram fdecl =
    "\n\nSemantically checked program: \n\n" ^
    String.concat "\n" (List.map string_of_sstmt fdecl.sbody) ^
    "\n"
