(* Semantic checking for the FunC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each statement in program.body *)

let check (program) =

  let check_program program =
    let check_expr = function
          Literal l -> (Int, SLiteral l)
        | BoolLit l -> (Bool, SBoolLit l)
        | StrLit l -> (String, SStrLit l)

    in

    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then () else raise (Failure err)
    in
    let check_defn t id value =
        let (rt, e') = check_expr value in
        let err = "illegal assignment " ^ string_of_typ t ^ " = " ^
                  string_of_typ rt ^ " in " ^ string_of_bind (Defn(t, id, value))
        in
        check_assign t rt err;
        SDefn(t, id, (rt, e'))
    in

    let rec check_bind = function
          Decl(typ, name) -> SDecl(typ, name)
          | Defn(t, id, value) -> check_defn t id value
    in

    let rec check_stmt_list = function
          [] -> []
        | Block sl :: sl'  -> check_stmt_list (sl @ sl') (* Flatten blocks *)
        | s :: sl -> check_stmt s :: check_stmt_list sl

    and check_stmt = function
      (* A block is correct if each statement is correct and nothing
          follows any Return statement.  Nested blocks are flattened. *)
      Block sl -> SBlock (check_stmt_list sl)
      | Expr e -> SExpr (check_expr e)
      | Bind b -> SBind (check_bind b)
    in
    {
      sbody = check_stmt_list program.body
    }
in
(check_program program)
