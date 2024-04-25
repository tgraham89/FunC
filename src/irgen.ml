module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate ({sbody} as program) =
  let context    = L.global_context () in

  let the_module = L.create_module context "FunC" in

  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and void_t     = L.void_type   context in

  (* Types *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> L.float_type context
    (* Add other types as necessary *)
  in

  (* Initialize and add global variables to the LLVM module *)
  let init_global_vars (stmts : sstmt list) : L.llvalue StringMap.t =
    let add_global m = function
      | SBind (SDecl (typ, name)) ->
        let llvm_type = ltype_of_typ typ in
        let init_val = L.const_int llvm_type 0 in
        StringMap.add name (L.define_global name init_val the_module) m
      (* TODO: This initializes globals that are assigned to 0, need to evaluate expression and assign it to init_val *)
      | SBind (SDefn (typ, name, _)) ->
        let llvm_type = ltype_of_typ typ in
        let init_val = L.const_int llvm_type 0 in
        StringMap.add name (L.define_global name init_val the_module) m
      | _ -> m
    in
    List.fold_left add_global StringMap.empty stmts
  in
  let global_vars = init_global_vars sbody in 

  (* Define the print function *)
   let print_func =
  let printf_ty = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  L.declare_function "printf" printf_ty the_module in

  (* Define the main function *)
  let main_func = 
    let main_ty = L.function_type i32_t [||] in
    L.define_function "main" main_ty the_module in
    let the_function = main_func in
    let builder = L.builder_at_end context (L.entry_block main_func) in

  (* Define a function to print a string *)
  let print_string_fn = 
    let print_str_ty = L.function_type void_t [| L.pointer_type i8_t |] in
    L.define_function "print" print_str_ty the_module in
    let print_string_builder = L.builder_at_end context (L.entry_block print_string_fn) in
    let str_arg = L.param print_string_fn 0 in
    let print_format_str = L.build_global_stringptr "%s\n" "fmt" print_string_builder in
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    ignore(L.build_call print_string_fn [| print_format_str; str_arg |] "" print_string_builder);
    ignore(L.build_ret_void print_string_builder);

   (* Define a function to print a string *)
  let print_int_fn =
    let print_int_ty = L.function_type void_t [| i32_t |] in
    L.define_function "print_int" print_int_ty the_module in
    let print_int_builder = L.builder_at_end context (L.entry_block print_int_fn) in
    let int_arg = L.param print_int_fn 0 in
    let print_format_str = L.build_global_stringptr "%d\n" "fmt" print_int_builder in
    ignore(L.build_call print_func [| print_format_str; int_arg |] "" print_int_builder);
    ignore(L.build_ret_void print_int_builder);

let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

(* Return the value for a variable or formal argument.
       Check local names first, then global names *)
       (* ONLY LOOKS IN GLOBAL CONTEXT RIGHT NOW *)
    let lookup n = try StringMap.find n global_vars
      with Not_found -> StringMap.find n global_vars
    in

let rec gen_stmt builder = function
  | SExpr expr ->
    ignore(gen_expr builder expr); builder
  | SBind bind ->
    gen_bind builder bind; builder
  | SBlock stmts ->
    List.fold_left gen_stmt builder stmts
  | SIf (cond, then_stmt) ->
    gen_if_stmt builder cond then_stmt None; builder
  | SIfElse (cond, then_stmt, else_stmt) ->
    gen_if_stmt builder cond then_stmt (Some else_stmt); builder
  | SWhile (cond, body) ->
    gen_while_stmt builder cond body; builder
  | SFor (init, cond, step, body) ->
    gen_for_stmt builder init cond step body; builder
  | SReturn expr ->
    gen_return builder expr; builder
  | SStructDecl _ -> builder
  | stmt ->
    raise (Failure ("Unhandled statement: " ^ string_of_sstmt stmt))

  and gen_bind builder = function
  | SDecl (_, _) -> builder (* Global variable declarations are handled elsewhere *)
  | SDefn (_, _, _) -> builder (* Global variable definitions are handled elsewhere *)

  and gen_expr builder = function
  | (_, SUnaryOp (a, b)) ->  raise (Failure "UnaryOp not implemented yet")
  | (_, SLiteral i) -> L.const_int i32_t i
  | (_, SBoolLit b) -> L.const_int i1_t (if b then 1 else 0)
  | (_, SStrLit s) -> L.build_global_stringptr s "str" builder
  | (_, SChrLit c) -> raise (Failure "SChrLit not implemented yet")
  | (_, SFloatLit c) -> raise (Failure "SFloatLit not implemented yet")
  | (_, SId id) ->
    let val_ptr = lookup_variable id global_vars in
    L.build_load val_ptr id builder
  | (_, SBinop (a, op, b)) -> raise (Failure "SBinop not implemented yet")
  | (_, SAssign (s, e)) -> let e' = gen_expr builder e in
        ignore(L.build_store e' (lookup s) builder); e'
  | (_, SListLit (a, b)) -> raise (Failure "SListLit not implemented yet")
  | (_, SFunction (a, b)) -> raise (Failure "SFunction not implemented yet")
  | (_, SCall (callee, args)) ->
    let callee_func =
      match callee with
      | (_, SId "print") -> (* Handle print function ONLY INT FOR NOW *)
        print_endline (string_of_sexpr_list "," args);
         List.iter (fun arg ->
          let arg_val = gen_expr builder arg in
          ignore (L.build_call print_int_fn [| arg_val |] "print_int" builder)
        ) args;
        L.const_int i32_t 0
        (* raise (Failure ("In print")) *)
        | (_, SId name) -> 
          (match L.lookup_function name the_module with
          | Some func -> func
          | None -> raise (Failure ("Function not found: " ^ name)))
      | _ -> raise (Failure "Only function identifiers can be called")
    in
    let llvm_args = Array.of_list (List.map (gen_expr builder) args) in
    L.build_call callee_func llvm_args "%s\n" builder
  | (_, SStructId s) -> raise (Failure "SStructId not implemented yet")
   | (_, SStructAccess s) -> raise (Failure "SStructAccess not implemented yet")
  | (_, SStructAssign s) -> raise (Failure "SStructAssign not implemented yet") 
  | _ -> raise (Failure ("gen_expr match not found"))
  (* Handle other cases as needed *)

and lookup_variable name global_vars =
  StringMap.find name global_vars

and gen_if_stmt builder cond then_stmt maybe_else_stmt =
  let cond_val = gen_expr builder cond in
  let then_bb = L.append_block context "then" the_function in
  let else_bb = L.append_block context "else" the_function in
  let merge_bb = L.append_block context "merge" the_function in

  let start_bb_builder = L.builder_at_end context (L.insertion_block builder) in
  ignore (L.build_cond_br cond_val then_bb else_bb start_bb_builder);

  let then_builder = L.builder_at_end context then_bb in
  let then_builder = gen_stmt then_builder then_stmt in
  ignore (L.build_br merge_bb then_builder);

  let else_builder =
    match maybe_else_stmt with
    | Some else_stmt ->
      let else_builder = L.builder_at_end context else_bb in
      let else_builder = gen_stmt else_builder else_stmt in
      ignore (L.build_br merge_bb else_builder)
    | None ->
      let else_builder = L.builder_at_end context else_bb in
      ignore (L.build_br merge_bb else_builder)
  in
  L.builder_at_end context merge_bb

and gen_while_stmt builder cond body =
  let cond_bb = L.append_block context "while_cond" the_function in
  let body_bb = L.append_block context "while_body" the_function in
  let merge_bb = L.append_block context "merge" the_function in
  ignore (L.build_br cond_bb builder);
  let cond_builder = L.builder_at_end context cond_bb in
  let cond_val = gen_expr cond_builder cond in
  ignore (L.build_cond_br cond_val body_bb merge_bb cond_builder);
  let body_builder = L.builder_at_end context body_bb in
  let body_builder = gen_stmt body_builder body in
  ignore (L.build_br cond_bb body_builder);
  L.builder_at_end context merge_bb

and gen_for_stmt builder init cond step body = builder
 (* To be implemented *)
 
and gen_return builder expr =
  let ret_val = gen_expr builder expr in
  ignore (L.build_ret ret_val builder) in

  (* List.iter (gen_stmt builder) sbody; *)
  let builder = L.builder_at_end context (L.entry_block main_func) in
  let final_builder = List.fold_left gen_stmt builder sbody in

  ignore(L.build_ret (L.const_int i32_t 0) builder);
  the_module

