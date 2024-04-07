(* Semantic checking for the FunC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each statement in program.body *)

(* Debugger Functions *)
let map_to_str m = 
  let inners = List.map (fun (k, v) -> k ^ " -> " ^ (string_of_typ v)) (StringMap.bindings m)
  in "[" ^ (String.concat ", " inners) ^ "]"
  

let print_members = function
  Decl (t, s) -> begin print_string(string_of_typ t); print_endline(" " ^ s); end
  | Defn (t, s, e) -> print_endline ("definition")

let print_all_members b = List.map(print_members) b

let print_members_list i = match i with
  Assign (s, e) -> begin print_string s; print_endline(" " ^ string_of_expr e); end
  | _ -> print_endline (string_of_expr i)

let print_all_members_list b = List.map(print_members_list) b

(* This helper function checks if the names in the 
   struct instance match the struct declaration. *)
let compare_struct_decl_assign decl assign = match assign with
  StructAssign(a) -> let rec cmp decl this_assign = 
    match (decl, this_assign) with
      | [], [] -> true
      | [], _ -> raise (Failure "Trying to assign too many struct variables")
      | _, [] -> raise (Failure "Missing assignments in struct")
      | (h :: t), (hh :: tt) -> match (h, hh) with
            | Decl(typ, str), Assign(sstr, eexp) -> if str <> sstr then 
                                        raise (Failure "Struct variable names don't match")
                                        else cmp t tt
      | _ -> raise (Failure "Not comparing a struct")
    in cmp decl a
  | _ -> raise (Failure "Not comparing a struct")

(* End Debugger Functions *)

let check (program) = 
  let check_program program =

    let rec check_expr symbols = function
          Literal l -> (symbols, Int, SLiteral l)
          | UnaryOp(op, e) ->
            let (_, t, e') = check_expr symbols e in
            begin match (op, t) with
            | (Pos, Int) -> (symbols, Int, SUnaryOp(SPos, (Int, e')))
            | (Neg, Int) -> (symbols, Int, SUnaryOp(SNeg, (Int, e')))
            | (Pos, Float) -> (symbols, Float, SUnaryOp(SPos, (Float, e')))
            | (Neg, Float) -> (symbols, Float, SUnaryOp(SNeg, (Float, e')))
            | _ -> raise (Failure "unary operator only applicable to int or float")
            end
        | BoolLit l -> (symbols, Bool, SBoolLit l)
        | FloatLit l -> (symbols, Float, SFloatLit l)
        | StrLit l -> (symbols, String, SStrLit l)
        | ChrLit ch -> (symbols, Char, SChrLit ch)
        | ListLit x -> 
          let rec verify_list = function 
          [] -> (EmptyList, true) 
          | x :: [] -> let (_, tyx, _) = check_expr symbols x in (tyx, true) 
          | x :: rest -> 
            let (_, tyx, _) = check_expr symbols x 
            in (tyx, List.for_all (fun a -> let (_, ca, _) = check_expr symbols a in ca = tyx) rest) in
            let slist = List.map (fun (_, t, x) -> (t, x)) (List.map (check_expr symbols) x) in
            let (tylist, valid) = verify_list x in
            let slistlit = (symbols, tylist, SListLit(tylist, slist)) in
            if valid then slistlit else raise (Failure "the types of this list dont match")
        | Assign(var, e) as ex ->
          let lt = type_of_identifier symbols var
          and (symbols, rt, e') = check_expr symbols e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                    string_of_typ rt ^ " in " ^ string_of_expr ex in
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
            | (Add, t1, t2) -> (symbols, t1, SBinop ((t1, slhs), op, (t2, srhs))) 
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
            | (Equal, t1, t2) -> (symbols, Bool, SBinop ((t1, slhs), op, (t2, srhs)))
            | (Neq, t1, t2) -> (symbols, Bool, SBinop ((t1, slhs), op, (t2, srhs)))
            | _ -> raise (Failure "not a binary operator")
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
              if expected_args <> arg_typs then raise (Failure (id ^ " expects arguments of type " ^ string_of_typ_list ", " expected_args ^ 
                " but arguments of type " ^ string_of_typ_list ", " arg_typs ^ 
                " were provided."))
            else (symbols, ret, SFuncInvoc (id, sexpr_list))
          | _ -> raise (Failure "This should only involve function signatures")
          end
        | Function(args, body) -> 
            let deduced_type = FunSig(type_arg_decl_list symbols args, typ_of_func_body symbols body)
            and sfunc = SFunction(check_bind_list symbols args, snd (check_stmt_list symbols body)) in
          (symbols, deduced_type, sfunc)
        | StructAssign(exprs) -> 
          (* begin
          print_all_members_list exprs;
          raise (Failure "EXIT"); 
          end *)
          let expr_members = function (* function to return assignment for 1 expression *)
            Assign(s, e) -> let (syms, typ, sx) = check_expr symbols e in
            (* print_endline(s); *)
            let (sexp : Sast.sexpr) = (typ, sx) in sexp
            | _ -> raise (Failure "Incorrect statement within struct instance creation") in
            (* apply function to each expression in the struct *)
            let expr_all_members all_sx = List.map(expr_members) all_sx in 
            let (sexprs : Sast.sexpr list) = expr_all_members exprs in
            (symbols, Struct, SStructAssign(sexprs))
        | Zero -> raise (Failure "need to figure out what to do with zero")
        | StructId (x) -> raise (Failure "StructId not implemented yet")
        | StructAccess (x, y) -> raise (Failure "StructAccess not implemented yet")
    and check_expr_list symbols lst = 
      let help = List.map (check_expr symbols) lst 
      in List.map (fun (_, x, y) -> (x, y)) help

      (* takes in a list of exprs from the arguments of a function call and extracts their type *)
    and typ_arg_list symbols lst = 
      List.map (fun (_, x, _) -> x) (List.map (check_expr symbols) lst)

      (* takes in an expr and returns its type *)
    and typ_of_expr symbols ex = 
      let (_, x, _) = check_expr symbols ex in x

      (* takes an identifier and returns its type *)
    and type_of_identifier symbols s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))

      (* takes in a list of arguments of a function definition/declaration and
         returns their types *)
    and type_arg_decl_list symbols lst = 
      (* Brendan: I think that this code is innocuous, but unnecessary *)
      let filtered = let rec h = function 
        [] -> [] | x :: rest -> begin 
                                  match x with 
                                  Decl(_, _) -> x :: (h rest) 
                                  | _ -> h rest 
                                end in h lst in
      let sdecls = 
          List.map (fun x ->  begin 
                                match x with 
                                Decl(y, z) -> check_decl symbols y z 
                                | _ -> raise (Failure "shouldn't see this") 
                              end) filtered in
      let help x = 
      begin
        match x with
        (_, SDecl(t, _)) -> t | _ -> raise (Failure "Function argument declarations must look like \"int x\"")
      end
    in List.map help sdecls

    (* checks for duplicate bindings *)
    and check_duplicate_binds symbols id =
      try
        ignore (StringMap.find id symbols);
        raise (Failure ("that bind id already exists " ^ id))
      with Not_found -> ()

      (* checks the types of an assignment *)
    and check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then () else raise (Failure err)

      (* This helper function validates the assignment of 
         varirables within a struct. The struct name is prepended
         to the variable name to differentiate *)
    and check_expr_struct symbols struct_name = function
      Assign(var, e) as ex ->
          let lt = type_of_identifier symbols (struct_name ^ "." ^ var)
          and (symbols, rt, e') = check_expr symbols e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                    string_of_typ rt ^ " in " ^ string_of_expr ex in
          check_assign lt rt err;
          (symbols, lt, SAssign(var, (rt, e')))
        | _ -> raise (Failure "Not a struct assignment")


      and check_struct_type_assign symbols value =
          check_expr symbols value

    (* This helper function checks if the types in the 
   struct instance match the struct declaration. *)
    and validate_struct_type symbols decl assign struct_name = 
      match assign with
      | StructAssign(e) -> let rec validate_helper s d a = match (d, a) with
          | [], [] -> true
          | [], _ -> raise (Failure "Trying to assign too many struct variables")
          | _, [] -> raise (Failure "Missing assignments in struct")
          | (h :: t), (hh :: tt) -> let (s, rt, e') = check_expr_struct symbols struct_name hh in
                match h with 
                  | Decl(typ, str) -> if typ <> rt then begin
                    (* print_endline("decl type was " ^ string_of_typ typ ^ ", assign type was " ^ string_of_typ rt); *)
                    raise (Failure "Missing assignments in struct"); end
                    else validate_helper s t tt
                  | _ -> raise (Failure "Not comparing a struct")
          (* | _ -> raise (Failure "Not comparing a struct") *)
              in validate_helper symbols decl e
          | _ -> raise (Failure "Not comparing a struct")

      (* Checks that struct members are in the expected format *)
    and check_struct_members symbols t value =
      let s = StringMap.find t symbols in
      match s with
      (* Pull name of struct and its members *)
        StructMem(name, members) -> begin 
          (* print_all_members members;  *)
          (* print_members_list value; *)
          (* Compare the struct assignment to its declaration *)
          compare_struct_decl_assign members value;
          check_struct_type_assign symbols value;
          validate_struct_type symbols members value t;
        end
        | _ -> raise (Failure "Not a struct but it looks like a struct")
    
      (* checks the actual type and the stated type of a definition *)
    and check_defn symbols t id value =
      let (symbols, rt, e') = check_expr symbols value in
      let err = "illegal assignment! " ^ string_of_typ t ^ " = " ^
                string_of_typ rt ^ " in " ^ string_of_bind (Defn(t, id, value))
      in
      (* If it's a struct, the Struct name should be a key in the symbol table *)
      if StringMap.mem (string_of_typ t) symbols then
        begin
        check_duplicate_binds symbols id;
        (* Check that struct members in definition match struct members in the type declaration *)
        check_struct_members symbols (string_of_typ t) value;
        let symbols = StringMap.add id rt symbols
         in
         (* Add a struct in with the type of the struct name *)
        (symbols, SDefn(t, id, (t, e')))
        end
      else
        begin 
        check_assign t rt err;
        check_duplicate_binds symbols id;
        let symbols = StringMap.add id t symbols
       in
      (symbols, SDefn(t, id, (rt, e')))
      end
    and check_decl symbols t id =
      check_duplicate_binds symbols id;
      let symbols = StringMap.add id t symbols in
      (symbols, SDecl(t, id))

    and check_bind symbols = function
      Decl(t, id) -> begin
        (* print_endline (map_to_str symbols); (* Print statement should be removed *) *)
        check_decl symbols t id;
      end
      | Defn(t, id, value) -> begin
        (* print_endline (map_to_str symbols); (* Print statement should be removed *) *)
        check_defn symbols t id value;
      end

    and check_bind_list symbols lst = List.map snd (List.map (check_bind symbols) lst)

    (* Adds each bind (decl) within a struct to the symbol table. Names of a variable
    within a struct are appended to the name of the struct *)
    and add_binds_to_table struct_name symbols = function
      Decl (t, id) -> let key = (struct_name ^ "." ^ id) in
      let value = t in
      check_duplicate_binds symbols id;
      StringMap.add key value symbols
      | _ -> raise (Failure "You should not be including a variable initialization in a structure definition")
    (* Add a bind list within a struct to the symbol table *)
    and check_bind_list_struct symbols struct_name lst = 
      (* Create partial to add in struct name *)
      let add_binds_to_spec_table = add_binds_to_table struct_name in
      (* Add every bind of the struct into the symbol table *)
      List.fold_left add_binds_to_spec_table symbols lst

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
          let (symbols2, b_checked) = check_bind symbols b in
          (symbols2, SBind b_checked)
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
      | For (counter, cond, increment, stmts) -> 
        print_endline("inside for check");
        print_endline(string_of_bind counter);
        print_endline(string_of_expr cond);
        print_endline(string_of_expr increment);
        let (symbols2, sbind) = check_bind symbols counter in 
        let (_, t1, scond) = check_expr symbols2 cond in 
        if t1 != Bool then raise (Failure "condition must be a boolean")
        else
          let (_, t2, sinc) = check_expr symbols2 increment in
          let (symbols2, sstmts) = check_stmt symbols2 stmts in
          (symbols2, SFor(sbind, (t1, scond), (t2, sinc), sstmts)) 
      | Return x -> let (_, y, z) = check_expr symbols x in (symbols, SReturn((y, z)))
      | StructDecl (s) -> 
        let symbols2 = check_bind_list_struct symbols s.sname s.members in
          let bind_members = function
          Decl (t, s) -> let (sbind : Sast.sbind) = SDecl (t, s) in sbind
          | _ -> raise (Failure "Definition in a struct declaration") in
          let bind_all_members b = List.map(bind_members) b in
          let (sbind : Sast.sbind list) = bind_all_members(s.members) in 
          (* Add to struct type and members to symbol table *)
          let symbols = StringMap.add s.sname (StructMem(s.sname, s.members)) symbols2 in 
          (* print_endline (map_to_str symbols); (* Print statement should be removed *) *)
          (symbols, SStructDecl({sname = s.sname; members = sbind})) 
      (* | _ -> raise (Failure "The statement that was parsed hasn't been implemented yet") *)
    in
    let built_in_symbols =
      StringMap.add "print" (FunSig([String], Void)) StringMap.empty
    in
    let (symbols, sbody_checked) = check_stmt_list built_in_symbols program.body 
  in
    {
      sbody = sbody_checked
    }
  in
(check_program program)