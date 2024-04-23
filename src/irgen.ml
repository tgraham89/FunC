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
  and i1_t       = L.i1_type     context in

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

   (* Define the main function *)
  let main_func = 
    let main_ty = L.function_type i32_t [||] in
    L.define_function "main" main_ty the_module in
  let builder = L.builder_at_end context (L.entry_block main_func) in
  
  (* Add other code here *)
  
  (* Add a return statement to the main function *)
  ignore(L.build_ret (L.const_int i32_t 0) builder);

(*
  ///

  let gen_expr e = function
    | SUnaryOp (uop, e) -> match uop with
      | + -> ...
      | - -> ...
    | SLiteral i -> L.const_int i32_t i
    | SBoolLit b -> L.const_int i1_t b
    | SStrLit s -> ...
    | SChrLit c -> ...
    | SFloatLit f -> ...
    | SId s -> ...
    | SBinop (e1, bop, e2) -> ...
    | SAssign (s, e) ->
    | SListLit (t, el) ->
    | SFunction (bl, sl) -> generate random name, call build_function
    | SCall (e, el) -> ...
    | SStructId s -> ...
    | SStructAccess e -> ...
    | SStructAssign el -> ...
    | SZero

  let gen_bind b = function
    | SDecl (t, s) -> 
      let llvm_type = ltype_of_typ typ in
      let init_val = L.const_int llvm_type 0 in
      ...
    | SDecl (t, s, e) ->
      match e with
        SFunction (args, block) -> generate function with s as name
      | _ -> gen_expr e
      ...
  
  let gen_stmt s = function
    | SBlock sl -> ...
    | SExpr e -> gen_expr e
    | SBind b -> gen_bind b
    | SIf (cond, block) -> ...
    | SIfElse (cond, block1, block2) -> ...
    | SWhile (cond, block) -> ...
    | SFor (init, cond, change, block) -> ...
    | SReturn e -> ...
    | SStructDecl

  
  ///
  *)
  the_module
