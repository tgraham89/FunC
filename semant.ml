(* Semantic checking for the FunC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each statement in program.body *)

let check (program) =

  let check_program program =

    let rec check_expr symbols = function
          Literal l -> (symbols, Int, SLiteral l)
        | BoolLit l -> (symbols, Bool, SBoolLit l)
        | StrLit l -> (symbols, String, SStrLit l)
        | Assign(var, e) as ex ->
          let lt = type_of_identifier symbols var
          and (symbols, rt, e') = check_expr symbols e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                    string_of_typ rt ^ " in " ^ string_of_expr ex
          in
          check_assign lt rt err;
          (symbols, lt, SAssign(var, (rt, e')))

    and type_of_identifier symbols s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))

    and check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then () else raise (Failure err)

    and check_defn symbols t id value =
      let (symbols, rt, e') = check_expr symbols value in
      let err = "illegal assignment " ^ string_of_typ t ^ " = " ^
                string_of_typ rt ^ " in " ^ string_of_bind (Defn(t, id, value))
      in
      check_assign t rt err;
      let symbols = StringMap.add id t symbols in
      (symbols, SDefn(t, id, (rt, e')))

    and check_decl symbols t id =
      let symbols = StringMap.add id t symbols in
      (symbols, SDecl(t, id))

    and check_bind symbols = function
      Decl(t, id) -> check_decl symbols t id
      | Defn(t, id, value) -> check_defn symbols t id value

    and check_stmt_list symbols = function
      | [] -> (symbols, []) 
      | s :: sl ->
          let (symbols, s_checked) = check_stmt symbols s in
          let (symbols, sl_checked) = check_stmt_list symbols sl in
          (symbols, s_checked :: sl_checked)
      (* TODO figure out how to do Block *)

    and check_stmt symbols = function
      Block sl -> 
          let (symbols, sl_checked) = check_stmt_list symbols sl in
          (symbols, SBlock sl_checked)
      | Expr e -> 
          let (symbols, t, e_checked) = check_expr symbols e in
          (symbols, SExpr(t, e_checked))
      | Bind b -> 
          let (symbols, b_checked) = check_bind symbols b in
          (symbols, SBind b_checked)

    in

    let (symbols, sbody_checked) = check_stmt_list StringMap.empty program.body in
    {
      sbody = sbody_checked
    }
in
(check_program program)
