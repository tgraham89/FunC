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


let rec list_map_to_str = function
  | [] -> "empty"
  | [map] -> map_to_str map
  | map :: tail -> map_to_str map ^ " , " ^ list_map_to_str tail

let print_members = function
  Decl (t, s) -> begin print_string(string_of_typ t); print_endline(" " ^ s); end
  | Defn (t, s, e) -> print_endline ("definition")

let print_all_members b = List.map(print_members) b

let print_members_list i = match i with
  Assign (Id(s), e) -> begin print_string s; print_endline(" " ^ string_of_expr e); end
  | _ -> print_endline (string_of_expr i)

let print_all_members_list b = List.map(print_members_list) b

(* This helper function checks if the names in the 
   struct instance match the struct declaration. *)
let compare_struct_decl_assign decl assign = match assign with
  StructAssign(a) -> let rec cmp decl this_assign = 
    match (decl, this_assign) with
      | [], [] -> ()
      | [], _ -> raise (Failure "Trying to assign too many struct variables")
      | _, [] -> raise (Failure "Missing assignments in struct")
      | (h :: t), (hh :: tt) -> match (h, hh) with
            | Decl(typ, str), Assign(Id(sstr), eexp) -> if str <> sstr then 
                                        raise (Failure "Struct variable names don't match")
                                        else cmp t tt
      | _ -> raise (Failure "Not comparing a struct")
    in cmp decl a
  | _ -> raise (Failure "Not comparing a struct")

(* End Debugger Functions *)

(* scope helper functions *)

let find_in_scope id scope =
  StringMap.find id scope


let rec find_in_scopes id = function
  | [] -> raise (Failure ("Undeclared identifier " ^ id))
  | scope :: tail -> 
    (* print_endline("finding "  ^ id ^ " in scopes: " ^ list_map_to_str [scope]); *)
    (  
      try find_in_scope id scope
      with Not_found -> find_in_scopes id tail
    )

let rec exist_in_scopes id = function
  | [] -> false
  | scope :: tail -> 
    if StringMap.mem id scope then true
    else exist_in_scopes id tail

let add_to_top_scope id typ scopes=
  (* print_endline("adding " ^ id ^ " -> " ^ string_of_typ typ ^ " to " ^ list_map_to_str scopes); *)
  match scopes with
  | [] -> raise (Failure "Scopes list should never be empty 2")
  | top_scope :: tail ->
      let updated_scope = StringMap.add id typ top_scope in
      updated_scope :: tail

let init_new_scope scopes = StringMap.empty :: scopes

let finish_scope = function
  | [] -> raise (Failure "No scopes exist")
  | _ :: lower_scopes -> lower_scopes

let check (program) = 
  let check_program program =

    let rec check_expr scopes = function
          Literal l -> (scopes, Int, SLiteral l)
          | UnaryOp(op, e) ->
            let (_, t, e') = check_expr scopes e in
            begin match (op, t) with
            | (Pos, Int) -> (scopes, Int, SUnaryOp(SPos, (Int, e')))
            | (Neg, Int) -> (scopes, Int, SUnaryOp(SNeg, (Int, e')))
            | (Pos, Float) -> (scopes, Float, SUnaryOp(SPos, (Float, e')))
            | (Neg, Float) -> (scopes, Float, SUnaryOp(SNeg, (Float, e')))
            | (Bang, Bool) -> (scopes, Bool, SUnaryOp(SBang, (Bool, e')))
            | (Pos, _) -> raise (Failure "+ unary operator only applicable to int or float")
            | (Neg, _) -> raise (Failure "- unary operator only applicable to int or float")
            | (Bang, _) -> raise (Failure "! unary operator only applicable to bool")
            end
        | BoolLit l -> (scopes, Bool, SBoolLit l)
        | FloatLit l -> (scopes, Float, SFloatLit l)
        | StrLit l -> (scopes, String, SStrLit l)
        | ChrLit ch -> (scopes, Char, SChrLit ch)
        | ListLit x ->
          let rec verify_list = function 
          [] -> (EmptyList, true)
          | x :: [] -> let (_, tyx, _) = check_expr scopes x in (tyx, true)
          | x :: rest ->
            let (_, tyx, _) = check_expr scopes x 
            in (tyx, List.for_all (fun a -> let (_, ca, _) = check_expr scopes a in ca = tyx) rest) in
            let slist = List.map (fun (_, t, x) -> (t, x)) (List.map (check_expr scopes) x) in
            let (tylist, valid) = verify_list x in
            (* let slistlit = (scopes, tylist, SListLit(tylist, slist)) in *)
            if valid then (scopes, List(tylist), SListLit(tylist, slist)) else raise (Failure "the types of this list dont match")
        (* | ListComp (Literal n, ex) -> let nt = typ_of_expr scopes n in *)
        | ListComp (n, v) ->
          let (nt, vt) = (typ_of_expr scopes n, typ_of_expr scopes v) in
          check_assign nt Int "number of items must be an int";
          (* let rec replicate x = function m when m > 0 -> (vt, x) :: replicate x (m - 1) | _ -> [] in *)
          let (_, tyx, x) = check_expr scopes v in 
          let (_, tn, ts) = check_expr scopes n in
          (scopes, List(vt), SListComp((tn, ts), (tyx, x)))
        | Assign(Index(Id id, i), e) ->
          let lt = begin match (type_of_identifier scopes id) with List t -> t | _ -> raise (Failure "must be a list") end in
          let (_, idt, id') = check_expr scopes (Index(Id id, i)) in
          let (_, rt, e') = check_expr scopes e in 
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                    string_of_typ rt ^ " in " ^ string_of_expr e in
          check_assign lt rt err;
          (scopes, lt, SAssign((idt, id'), (rt, e')))
      | Assign(Id(id), e) as ex ->
          let lt = (type_of_identifier scopes id) in
          let (_, rt, e') = check_expr scopes e in
          let (_, idt, id') = check_expr scopes (Id id) in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                    string_of_typ rt ^ " in " ^ string_of_expr ex in
          check_assign lt rt err;
          (scopes, lt, SAssign((idt, id'), (rt, e')))
        | Id id -> (scopes, type_of_identifier scopes id, SId id)
        | Binop (lhs, op, rhs) -> let (_, t1, slhs) = check_expr scopes lhs and (_, t2, srhs) = check_expr scopes rhs in
          begin
          match (op, t1, t2) with
              (_, t1, t2) when t1 != t2 -> raise (Failure "mismatched types")
            | (Add, t1, t2) when t1 != Int && t1 != Float -> raise (Failure "can't add non-numeric things")
            | (Sub, t1, t2) when t1 != Int && t1 != Float -> raise (Failure "can't subtract non-numeric things")
            | (Mult, t1, t2) when t1 != Int && t1 != Float -> raise (Failure "can't multiply non-numeric things")
            | (Div, t1, t2) when t1 != Int && t1 != Float -> raise (Failure "can't divide non-numeric things")
            | (Mod, t1, t2) when t1 != Int && t1 != Float -> raise (Failure "can't mod non-numeric things")
            | (Or, t1, t2) when t1 != Bool -> raise (Failure "Or only accepts booleans")
            | (And, t1, t2) when t1 != Bool -> raise (Failure "And only accepts booleans")
            | (Add, t1, t2) -> (scopes, t1, SBinop ((t1, slhs), op, (t2, srhs))) 
            | (Sub, t1, t2) -> (scopes, t1, SBinop ((t1, slhs), op, (t2, srhs))) 
            | (Mult, t1, t2) -> (scopes, t1, SBinop ((t1, slhs), op, (t2, srhs))) 
            | (Div, t1, t2) -> (scopes, t1, SBinop ((t1, slhs), op, (t2, srhs))) 
            | (Mod, t1, t2) -> (scopes, t1, SBinop ((t1, slhs), op, (t2, srhs))) 
            | (Or, t1, t2) -> (scopes, Bool, SBinop ((t1, slhs), op, (t2, srhs))) 
            | (And, t1, t2) -> (scopes, Bool, SBinop ((t1, slhs), op, (t2, srhs)))
            | (Greater, t1, t2) -> (scopes, Bool, SBinop ((t1, slhs), op, (t2, srhs)))
            | (Gequal, t1, t2) -> (scopes, Bool, SBinop ((t1, slhs), op, (t2, srhs)))
            | (Less, t1, t2) -> (scopes, Bool, SBinop ((t1, slhs), op, (t2, srhs)))
            | (Lequal, t1, t2) -> (scopes, Bool, SBinop ((t1, slhs), op, (t2, srhs)))
            | (Equal, t1, t2) -> (scopes, Bool, SBinop ((t1, slhs), op, (t2, srhs)))
            | (Neq, t1, t2) -> (scopes, Bool, SBinop ((t1, slhs), op, (t2, srhs)))
            | _ -> raise (Failure "not a binary operator")
          end
        | Call (Id("print"), [arg]) ->
          let (_, arg_type, sarg) = check_expr scopes arg in
          begin
          match arg_type with
            Int -> (scopes, Void, SCall ((FunSig([Int], Void), SId("print_int")), [(arg_type, sarg)]))
            | String -> (scopes, Void, SCall ((FunSig([String], Void), SId("print_str")), [(arg_type, sarg)]))
            | Float -> (scopes, Void, SCall ((FunSig([Float], Void), SId("print_float")), [(arg_type, sarg)]))
            | _ -> raise (Failure ("print not implemented for type: " ^ string_of_typ arg_type))
          end
        | Call (callable, args) -> 
          let signature = typ_of_expr scopes callable
          and num_args = List.length args 
          and arg_typs = typ_arg_list scopes args 
          and (_, call_type, scallable) = check_expr scopes callable
          and sexpr_list = check_expr_list scopes args in 
          begin
          match (signature, args, num_args, arg_typs) with 
          (FunSig(expected_args, _), _, num_args, _) when num_args != (List.length expected_args) -> 
            raise (Failure ("Expects " ^ string_of_int (List.length expected_args) ^ 
            " but " ^ string_of_int (List.length expected_args) ^ " were provided."))
          | (FunSig(expected_args, ret), args, _, arg_typs) ->
              if expected_args <> arg_typs then raise (Failure ("Expects arguments of type " ^ string_of_typ_list ", " expected_args ^ 
                " but arguments of type " ^ string_of_typ_list ", " arg_typs ^ 
                " were provided."))
            else (scopes, ret, SCall ((call_type, scallable), sexpr_list))
          | _ -> raise (Failure "This should only involve function signatures")
          end
        | Function(args, body) ->
            let scopes = init_new_scope scopes in
            let (scopes, arg_bind_list) = check_bind_list scopes args in
            let arg_types = 
              let help x =
              begin
                match x with
                SDecl(t, _) -> t | _ -> raise (Failure "Function argument declarations must look like \"int x\"")
              end
              in List.rev (List.map help arg_bind_list) in
            let scopes, sbody = check_stmt_list scopes body in
            let sfunc = SFunction(arg_bind_list, sbody) in
            let deduced_type = FunSig(arg_types, typ_of_func_body scopes body)
          in
          (finish_scope scopes, deduced_type, sfunc)
        | StructAssign(exprs) ->
          let expr_members = function (* function to return assignment for 1 expression *)
            Assign(s, e) -> let (syms, typ, sx) = check_expr scopes e in
            (* print_endline(string_of_typ typ); *)
            let (sexp : Sast.sexpr) = (typ, sx) in sexp
            | _ -> raise (Failure "Incorrect statement within struct instance creation") in
            (* apply function to each expression in the struct *)
            let expr_all_members all_sx = List.map(expr_members) all_sx in 
            let (sexprs : Sast.sexpr list) = expr_all_members exprs in
            (scopes, Struct, SStructAssign(sexprs))
        | Zero -> raise (Failure "need to figure out what to do with zero")
        | StructId (id) -> (scopes, type_of_identifier scopes id, SStructId id)
        | StructAccess (str) -> let (_, typ, str') = check_expr scopes str in
          (scopes, typ, SStructId (string_of_expr str))
        | Index (Id id, i) ->  begin match type_of_identifier scopes id with 
          List x -> let (_, s, sid) = (check_expr scopes (Id id)) in 
                    let (_, r, sindex) = (check_expr scopes i) in
          (scopes, x, SIndex ((s, sid), (r, sindex))) | _ -> raise (Failure "can only index lists") end
        | _ -> raise (Failure "Unknown Expression - semantic analysis phase") 
    and check_expr_list scopes lst = 
      let help = List.map (check_expr scopes) lst 
      in List.map (fun (_, x, y) -> (x, y)) help

      (* takes in a list of exprs from the arguments of a function call and extracts their type *)
    and typ_arg_list scopes lst = 
      List.map (fun (_, x, _) -> x) (List.map (check_expr scopes) lst)

      (* takes in an expr and returns its type *)
    and typ_of_expr scopes ex = 
      let (_, x, _) = check_expr scopes ex in x

      (* takes an identifier and returns its type *)
    and type_of_identifier scopes (s : string) =
      find_in_scopes s scopes

    (* checks for duplicate bindings in top scope*)
    and check_duplicate_binds scopes id =
      try
        ignore (find_in_scope id (List.hd scopes));
        raise (Failure ("that bind id already exists " ^ id))
      with Not_found -> ()

      (* checks the types of an assignment *)
    and check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then () else raise (Failure err)

      (* This helper function validates the assignment of 
         varirables within a struct. The struct name is prepended
         to the variable name to differentiate *)
    and check_expr_struct scopes struct_name = function
      Assign(Id var, e) as ex ->
          let lt = type_of_identifier scopes (struct_name ^ "." ^ var)
          and (_, rt, e') = check_expr scopes e 
          and (_, idt, id') = check_expr scopes (Id var) in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                    string_of_typ rt ^ " in " ^ string_of_expr ex in
          check_assign lt rt err;
          (* print_endline (map_to_str scopes); (* Print statement should be removed *) *)
          (scopes, lt, SAssign((idt, id'), (rt, e')))
        | _ -> raise (Failure "Not a struct assignment")


      and check_struct_type_assign scopes value =
          check_expr scopes value

    (* This helper function checks if the types in the 
   struct instance match the struct declaration. *)
    and validate_struct_type scopes decl assign struct_name struct_id = 
      match assign with
      | StructAssign(e) -> let rec validate_helper s d a = match (d, a) with
          | [], [] -> s
          | [], _ -> raise (Failure "Trying to assign too many struct variables")
          | _, [] -> raise (Failure "Missing assignments in struct")
          | (h :: t), (hh :: tt) -> let (s', rt, e') = check_expr_struct scopes struct_name hh in
                match h with 
                  | Decl(typ, str) -> if typ <> rt then begin
                    (* print_endline("decl type was " ^ string_of_typ typ ^ ", assign type was " ^ string_of_typ rt); *)
                    raise (Failure "Missing assignments in struct"); end
                    else 
                      let sym = add_to_top_scope (struct_id ^ "." ^ str) rt s in
                      validate_helper sym t tt
                  | _ -> raise (Failure "Not comparing a struct")
          (* | _ -> raise (Failure "Not comparing a struct") *)
              in validate_helper scopes decl e
      | _ -> raise (Failure "Not comparing a struct")

      (* Checks that struct members are in the expected format *)
    and check_struct_members scopes t value id =
      let s = find_in_scopes t scopes in
      match s with
      (* Pull name of struct and its members *)
        StructMem(name, members) -> begin 
          (* Compare the struct assignment to its declaration *)
          compare_struct_decl_assign members value;
          let _ = check_struct_type_assign scopes value in
          let scopes2 = validate_struct_type scopes members value t id in
          scopes2
        end
        | _ -> raise (Failure "Not a struct but it looks like a struct")
    
      (* checks the actual type and the stated type of a definition *)
    and check_defn scopes t id value =
      let (scopes, rt, e') = check_expr scopes value in
      let err = "illegal assignment! " ^ string_of_typ t ^ " = " ^
                string_of_typ rt ^ " in " ^ string_of_bind (Defn(t, id, value))
      in
      (* If it's a struct, the Struct name should be a key in the symbol table *)
      if exist_in_scopes (string_of_typ t) scopes then
        begin
        check_duplicate_binds scopes id;
        (* Check that struct members in definition match struct members in the type declaration *)
        let scopes2 = check_struct_members scopes (string_of_typ t) value id in
        let scopes = add_to_top_scope id t scopes2
         in
         (* Add a struct in with the type of the struct name *)
        (scopes, SDefn(t, id, (t, e')))
        end
      else
        begin 
        check_assign t rt err;
        check_duplicate_binds scopes id;
        let scopes = add_to_top_scope id t scopes
       in
      (scopes, SDefn(t, id, (rt, e')))
      end
    and check_decl scopes t id =
      check_duplicate_binds scopes id;
      let scopes = add_to_top_scope id t scopes in
      (scopes, SDecl(t, id))

    and check_bind scopes = function
      Decl(t, id) -> begin
        (* print_endline (map_to_str scopes); (* Print statement should be removed *) *)
        check_decl scopes t id;
      end
      | Defn(t, id, value) -> begin
        (* print_endline (map_to_str scopes); (* Print statement should be removed *) *)
        check_defn scopes t id value;
      end

    and check_bind_list scopes lst = 
      let process_bind (current_scopes, binds_acc) bind =
      let (updated_scopes, new_bind) = check_bind current_scopes bind in
      (updated_scopes, new_bind :: binds_acc)
    in
    let (final_scopes, reversed_sbinds) = List.fold_left process_bind (scopes, []) lst in
    (final_scopes, List.rev reversed_sbinds)

    (* Adds each bind (decl) within a struct to the symbol table. Names of a variable
    within a struct are appended to the name of the struct *)
    and add_binds_to_table struct_name scopes = function
      Decl (t, id) -> let key = (struct_name ^ "." ^ id) in
      let value = t in
      check_duplicate_binds scopes id;
      add_to_top_scope key value scopes
      | _ -> raise (Failure "You should not be including a variable initialization in a structure definition")
    (* Add a bind list within a struct to the symbol table *)
    and check_bind_list_struct scopes struct_name lst = 
      (* Create partial to add in struct name *)
      let add_binds_to_spec_table = add_binds_to_table struct_name in
      (* Add every bind of the struct into the symbol table *)
      List.fold_left add_binds_to_spec_table scopes lst

    and typ_of_func_body scopes = function
      [] -> Void
      | x :: [] -> begin match x with Return y -> typ_of_expr scopes y | _ -> Void end
      | x :: rest -> typ_of_func_body scopes rest

    and check_stmt_list scopes = function
      | [] -> (scopes, []) 
      | s :: sl ->
          let (scopes, s_checked) = check_stmt scopes s in
          let (scopes, sl_checked) = check_stmt_list scopes sl in
          (scopes, s_checked :: sl_checked)
      (* TODO figure out how to do Block *)

    and check_stmt scopes = function
      Block sl -> 
          let scopes = init_new_scope scopes in
          let (scopes, sl_checked) = check_stmt_list scopes sl in
          (finish_scope scopes, SBlock sl_checked)
      | Expr e -> 
          let (scopes, t, e_checked) = check_expr scopes e in
          (scopes, SExpr(t, e_checked))
      | Bind b ->
          let (scopes2, b_checked) = check_bind scopes b in
          (scopes2, SBind b_checked)
      | If (cond, br) -> let (_, t, scond) = check_expr scopes cond in
        if t = Bool then let (_, sbr) = check_stmt scopes br in (scopes, SIf((t, scond), sbr))
        else raise (Failure "condition must be a boolean")
      | IfElse (cond, br1, br2) -> let (_, t, scond) = check_expr scopes cond in
        if t = Bool then let (_, sbr1) = check_stmt scopes br1 and (_, sbr2) = check_stmt scopes br2 in
          (scopes, SIfElse ((t, scond), sbr1, sbr2)) else
        raise (Failure "condition must be a boolean")
      | While (cond, stmts) -> let (_, t, scond) = check_expr scopes cond in
          if t != Bool then raise (Failure "condition must be a boolean")
          else let (_, sstmts) = check_stmt scopes stmts in
          (scopes, SWhile ((t, scond), sstmts))
      | For (counter, cond, increment, stmts) -> 
        let (scopes2, sbind) = check_bind scopes counter in 
        let (_, t1, scond) = check_expr scopes2 cond in 
        if t1 != Bool then raise (Failure "condition must be a boolean")
        else
          let (_, t2, sinc) = check_expr scopes2 increment in
          let (scopes2, sstmts) = check_stmt scopes2 stmts in
          (scopes2, SFor(sbind, (t1, scond), (t2, sinc), sstmts)) 
      | Return x -> let (_, y, z) = check_expr scopes x in (scopes, SReturn((y, z)))
      | StructDecl (s) -> 
        let scopes2 = check_bind_list_struct scopes s.sname s.members in
          let bind_members = function
          Decl (t, s) -> let (sbind : Sast.sbind) = SDecl (t, s) in sbind
          | _ -> raise (Failure "Definition in a struct declaration") in
          let bind_all_members b = List.map(bind_members) b in
          let (sbind : Sast.sbind list) = bind_all_members(s.members) in 
          (* Add to struct type and members to symbol table *)
          let scopes = add_to_top_scope s.sname (StructMem(s.sname, s.members)) scopes2 in 
          (* print_endline (map_to_str scopes); (* Print statement should be removed *) *)
          (scopes, SStructDecl({sname = s.sname; members = sbind})) 
      (* | _ -> raise (Failure "The statement that was parsed hasn't been implemented yet") *)
    in
    let built_in_symbols = StringMap.empty in
    let scopes = init_new_scope [built_in_symbols]
    in
    let (_, sbody_checked) = check_stmt_list scopes program.body
  in
    {
      sbody = sbody_checked
    }
  in
(check_program program)
