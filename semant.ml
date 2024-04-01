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
        | FloatLit l -> (symbols, Float, SFloatLit l)
        | StrLit l -> (symbols, String, SStrLit l)
        | ChrLit ch -> (symbols, Char, SChrLit ch)
        | ListLit x -> 
          let rec verify_list = function 
          [] -> (EmptyList, true) 
          | x :: [] -> let (_, tyx, _) = check_expr symbols x in (tyx, true) 
          | x :: rest -> let (_, tyx, _) = check_expr symbols x in (tyx, List.for_all (fun a -> let (_, ca, _) = check_expr symbols a in ca = tyx) rest)
          in
          let slist = List.map (fun (_, t, x) -> (t, x)) (List.map (check_expr symbols) x)
          in
          let (tylist, valid) = verify_list x in if valid then (symbols, tylist, SListLit(tylist, slist)) else raise (Failure "the types of this list dont match")
        | Assign(var, e) as ex ->
          let lt = type_of_identifier symbols var
          and (symbols, rt, e') = check_expr symbols e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                    string_of_typ rt ^ " in " ^ string_of_expr ex
          in
          check_assign lt rt err;
          (symbols, lt, SAssign(var, (rt, e')))
        | Id id -> (symbols, type_of_identifier symbols id, SId id)
        | Binop (lhs, op, rhs) -> let (_, t1, slhs) = check_expr symbols lhs and (_, t2, srhs) = check_expr symbols rhs in
          begin
          match (op, t1, t2) with
              (_, t1, t2) when t1 != t2 -> raise (Failure "mismatched types")
            | (Add, t1, t2) when t1 != Int && t1 != Float -> raise (Failure "can't add non-numeric things")
            | (Sub, t1, t2) when t1 != Int && t1 != Float -> raise (Failure "can't subtract non-numeric things")
            | (Mult, t1, t2) when t1 != Int && t1 != Float -> raise (Failure "can't multiply non-numeric things")
            | (Div, t1, t2) when t1 != Int && t1 != Float -> raise (Failure "can't divide non-numeric things")
            | (Mod, t1, t2) when t1 != Int && t1 != Float -> raise (Failure "can't mod non-numeric things")
            | (Or, t1, t2) when t1 != Bool -> raise (Failure "Or only accepts booleans")
            | (And, t1, t2) when t1 != Bool -> raise (Failure "Or only accepts booleans")
            | (Sub, t1, t2) -> (symbols, t1, SBinop ((t1, slhs), op, (t2, srhs))) 
            | (Mult, t1, t2) -> (symbols, t1, SBinop ((t1, slhs), op, (t2, srhs))) 
            | (Div, t1, t2) -> (symbols, t1, SBinop ((t1, slhs), op, (t2, srhs))) 
            | (Mod, t1, t2) -> (symbols, t1, SBinop ((t1, slhs), op, (t2, srhs))) 
            | (Or, t1, t2) -> (symbols, Bool, SBinop ((t1, slhs), op, (t2, srhs))) 
            | (And, t1, t2) -> (symbols, Bool, SBinop ((t1, slhs), op, (t2, srhs)))
            | (Greater, t1, t2) -> (symbols, Bool, SBinop ((t1, slhs), op, (t2, srhs)))
            | (Gequal, t1, t2) -> (symbols, Bool, SBinop ((t1, slhs), op, (t2, srhs)))
            | (Less, t1, t2) -> (symbols, Bool, SBinop ((t1, slhs), op, (t2, srhs)))
            | (Lequal, t1, t2) -> (symbols, Bool, SBinop ((t1, slhs), op, (t2, srhs))) 
          end
        | FuncInvoc (id, args) -> 
          let signature = type_of_identifier symbols id 
          and num_args = List.length args 
          and arg_typs = typ_arg_list symbols args 
          and sexpr_list = check_expr_list symbols args in 
          begin
          match (signature, args, num_args, arg_typs) with 
          (FunSig(expected_args, _), _, num_args, _) when num_args != (List.length expected_args) -> 
            raise (Failure (id ^ " expects " ^ string_of_int (List.length expected_args) ^ 
            " but " ^ string_of_int (List.length expected_args) ^ " were provided."))
          | (FunSig(expected_args, ret), args, _, arg_typs) ->
              if args != args then raise (Failure (id ^ " expects arguments of type " ^ string_of_typ_list ", " expected_args ^ 
                " but arguments of type " ^ string_of_typ_list ", " arg_typs ^ 
                " were provided."))
            else (symbols, ret, SFuncInvoc (id, sexpr_list))
          end
        | Function(args, body) -> 
            let deduced_type = FunSig(type_arg_decl_list symbols args, typ_of_func_body symbols body)
            and sfunc = SFunction(check_bind_list symbols args, snd (check_stmt_list symbols body)) in
          (symbols, deduced_type, sfunc)

    and check_expr_list symbols lst = 
      let help = List.map (check_expr symbols) lst 
      in List.map (fun (_, x, y) -> (x, y)) help

    and typ_arg_list symbols lst = 
      List.map (fun (_, x, _) -> x) (List.map (check_expr symbols) lst)

    and typ_of_expr symbols ex = 
      let (_, x, _) = check_expr symbols ex in x
    and type_of_identifier symbols s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))

    and type_arg_decl_list symbols lst = 
      let filtered = let rec h = function 
        [] -> [] | x :: rest -> begin match x with Decl(_, _) -> x :: (h rest) | _ -> h rest end in h lst
      in
      let sdecls = List.map (fun x -> begin match x with Decl(y, z) -> check_decl symbols y z | _ -> raise (Failure "shouldn't see this") end) filtered in
      let help x = 
      begin
        match x with
        (_, SDecl(t, _)) -> t | _ -> raise (Failure "Function argument declarations must look like \"int x\"")
      end
    in List.map help sdecls

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

    and check_bind_list symbols lst = List.map snd (List.map (check_bind symbols) lst)

    and typ_of_func_body symbols = function
      [] -> Void
      | x :: [] -> begin match x with Return y -> typ_of_expr symbols y | _ -> Void end
      | x :: rest -> typ_of_func_body symbols rest

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
      | If (cond, br) -> let (_, t, scond) = check_expr symbols cond in
        if t = Bool then let (_, sbr) = check_stmt symbols br in (symbols, SIf((t, scond), sbr))
        else raise (Failure "condition must be a boolean")
      | IfElse (cond, br1, br2) -> let (_, t, scond) = check_expr symbols cond in
        if t = Bool then let (_, sbr1) = check_stmt symbols br1 and (_, sbr2) = check_stmt symbols br2 in
          (symbols, SIfElse ((t, scond), sbr1, sbr2)) else
        raise (Failure "condition must be a boolean")
      | While (cond, stmts) -> let (_, t, scond) = check_expr symbols cond in
          if t != Bool then raise (Failure "condition must be a boolean")
          else let (_, sstmts) = check_stmt symbols stmts in
          (symbols, SWhile ((t, scond), sstmts))
      | For (counter, cond, increment, stmts) -> let (_, t, scond) = check_expr symbols cond in
        if t != Bool then raise (Failure "condition must be a boolean")
        else
          let (_, sbind) = check_bind symbols counter and
              (_, t1, scond) = check_expr symbols cond and
              (_, t2, sinc) = check_expr symbols increment and
              (_, sstmts) = check_stmt symbols stmts in
              (symbols, SFor(sbind, (t1, scond), (t2, sinc), sstmts)) 
      | Return x -> let (_, y, z) = check_expr symbols x in (symbols, SReturn((y, z)))
      | _ -> raise (Failure "uhh")
    in
    let (symbols, sbody_checked) = check_stmt_list StringMap.empty program.body in
    {
      sbody = sbody_checked
    }
in
(check_program program)