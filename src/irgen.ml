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

  (* let function_decls : (L.llvalue * SFunction) StringMap.t =
    let function_decl m (bindlist, stmtlist) =
      (* TODO: our functions do not have a name so how do we do a string to function map? *)
      let name = "" 
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) bindlist)
        (* TODO: what is the return type of our functions? *)
      in let ftype = L.function_type (ltype_of_typ returntype) formal_types in
      StringMap.add name (L.define_function name ftype the_module, (bindlist, stmtlist)) m in
    List.fold_left function_decl StringMap.empty sbody 
  in *)
  
  the_module
