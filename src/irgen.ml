module L = Llvm
module A = Ast
open Sast

let anon_name length =
	Random.self_init (); (* Initialize the random number generator *)
	let rec aux n acc =
		if n = 0 then acc
		else
			let digit = Random.int 10 in (* Generate a random digit from 0 to 9 *)
			let acc = acc ^ string_of_int digit in (* Append the string representation of the digit *)
			aux (n - 1) acc
	in
	aux length ""

module StringMap = Map.Make(String)

type scope = {
	vars: (L.llvalue * L.lltype) StringMap.t;
	var_names: string list;
	parent: scope option;
}

let create_scope ?parent () = {
	vars = StringMap.empty;
	var_names = [];
	parent = parent;
}

let finish_scope scope = scope.parent

let define_scope_struct context =
	let struct_type = L.named_struct_type context "ScopeStruct" in
	let ptr_type = L.pointer_type (L.i8_type context) in  (* Generic pointer type *)
	let var_array_ptr_type = L.pointer_type ptr_type  (* Pointer to an array of generic pointers (i8**) 
	in L.struct_set_body struct_type [| ptr_type; var_array_ptr_type |] false;
	struct_type

let rec create_llvm_scope context builder scope scope_struct_type =
	let num_vars = List.length scope.var_names in
	let scope_val = L.build_malloc scope_struct_type "scope_instance" builder in

	(* Create a dynamic array of generic pointers based on num_vars *)
	let ptr_type = L.pointer_type (L.i8_type context) in
	let var_array_val = L.build_array_malloc ptr_type (L.const_int (L.i32_type context) num_vars) "vars_array" builder in

	(* Initialize the parent pointer *)
	let parent_ptr = match scope.parent with
			| Some parent_scope -> create_llvm_scope context builder parent_scope scope_struct_type
			| None -> L.const_null (L.pointer_type (L.i8_type context))
	in
	let parent_ptr_field = L.build_struct_gep scope_val 0 "parent_ptr" builder in
	ignore (L.build_store parent_ptr parent_ptr_field builder);
	(* Initialize variables array *)
	let idx = ref 0 in
	List.iter (fun var_name ->
			if !idx < num_vars then
					let var_ptr = L.build_gep var_array_val [| L.const_int (L.i32_type context) !idx |] "var_ptr" builder in
					let llvm_val = fst(StringMap.find var_name scope.vars) in
					let casted_val = L.build_bitcast llvm_val ptr_type "casted_val" builder in
					ignore (L.build_store casted_val var_ptr builder);
					incr idx
	) (List.rev scope.var_names);

	(* Store the array pointer in the scope structure *)
	let vars_ptr_field = L.build_struct_gep scope_val 1 "vars_array_ptr" builder in
	ignore (L.build_store var_array_val vars_ptr_field builder);

	scope_val

	

let gen_scope_var context builder scope_val var_index var_name expected_type =
	let vars_array_ptr = L.build_struct_gep scope_val 1 "vars_array_ptr" builder in
	let vars_array = L.build_load vars_array_ptr "vars_array" builder in
	let var_ptr = L.build_gep vars_array [| L.const_int (L.i32_type context) var_index |] (var_name ^ "_ptr") builder in
	let var_val = L.build_load var_ptr (var_name ^ "_load") builder in
	L.build_bitcast var_val (L.pointer_type expected_type) var_name builder

let get_llvm_function_type context module_builder scope_struct_type arg_types ret_type =
	let scope_ptr_type = L.pointer_type scope_struct_type in
	let func_type = L.function_type ret_type (Array.append [| scope_ptr_type |] arg_types) in
	func_type

let add_variable name (llvm_val, llvm_type) scope =
	let new_vars = StringMap.add name (llvm_val, llvm_type) scope.vars in
	let new_var_names = name :: scope.var_names in
	{ scope with vars = new_vars; var_names = new_var_names }

(* translate : Sast.program -> Llvm.module *)
let translate ({sbody}) =
	let context    = L.global_context () in

	let the_module = L.create_module context "FunC" in

	let i32_t      = L.i32_type    context
	and i8_t       = L.i8_type     context
	and i1_t       = L.i1_type     context
	and float_t    = L.double_type context
	(* and void_t     = L.void_type   context  *)
in
	let scope_struct_type = define_scope_struct context in
	(* Types *)
	let rec ltype_of_typ = function
			A.Int   -> i32_t
		| A.Bool  -> i1_t
		| A.Float -> float_t
		| A.String -> L.pointer_type i8_t 
		| A.FunSig(args_typ, ret_typ) -> 
			let lret_typ = ltype_of_typ ret_typ in
			let largs_typ = Array.of_list (List.map ltype_of_typ args_typ) in
			L.pointer_type (get_llvm_function_type context the_module scope_struct_type largs_typ lret_typ)
    | A.List(ty) -> L.pointer_type (ltype_of_typ ty)
    | _ -> raise (Failure ("type match not found"))
	in


	let printf_t : L.lltype =
		L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
	let printf_func : L.llvalue =
		L.declare_function "printf" printf_t the_module in

	let global_scope = create_scope () in

	(* let map_to_str m = 
		let inners = List.map (fun (k, v) -> k ^ " -> " ^ (string_of_int v)) (StringMap.bindings m)
		in "[" ^ (String.concat ", " inners) ^ "]" in *)

		
	(* Define the main function *)
	let main_func = 
		let main_ty = L.function_type i32_t [||] in
		L.define_function "main" main_ty the_module in
		let the_function = main_func in
		let builder = L.builder_at_end context (L.entry_block main_func) in


let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
let str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
let float_format_str = L.build_global_stringptr "%f\n" "fmt" builder in


let rec lookup name scope = 
	match StringMap.find_opt name scope.vars with
	| Some llvalue -> llvalue
	| None ->
		match scope.parent with
		| Some parent_scope -> lookup name parent_scope
		| None -> raise (Failure ("Variable not found: " ^ name)) in

let rec gen_outer_scope_vars scope scope_val builder =
	(* print_endline("num_vars: " ^ string_of_int num_vars); *)

	match scope.parent with
			| Some parent_scope -> 
				let parent_ptr = L.build_struct_gep scope_val 0 "parent_ptr" builder in
				let parent_val = L.build_load parent_ptr "parent_val" builder in
				gen_outer_scope_vars parent_scope parent_val builder;
			| None -> ();

	let num_vars = StringMap.cardinal scope.vars in
	let idx = ref 0 in
	List.iter (fun var_name ->
			if !idx < num_vars then
					let llvm_type = snd(lookup var_name scope) in
					ignore(gen_scope_var context builder scope_val !idx var_name llvm_type);
					incr idx
	) (List.rev scope.var_names);

in

let rec gen_stmt (builder, scope) = function
	| SExpr expr -> ignore(gen_expr builder scope expr); (builder, scope)
	| SBind bind ->
		gen_bind (builder, scope) bind
	| SBlock stmts -> let (bul, _) = List.fold_left gen_stmt (builder, scope) stmts in (bul, scope)
	| SIf (cond, then_stmt) ->
		gen_if_stmt builder scope cond then_stmt None; (builder, scope)
	| SIfElse (cond, then_stmt, else_stmt) ->
		gen_if_stmt builder scope cond then_stmt (Some else_stmt); (builder, scope)
	| SWhile (cond, body) ->
		gen_while_stmt builder scope cond body; (builder, scope)
	| SFor (init, cond, step, body) ->
		gen_for_stmt builder scope init cond step body; (builder, scope)
	| SStructDecl _ -> (builder, scope) 
	| SReturn expr ->
		gen_return builder scope expr; (builder, scope)


	and gen_bind (builder, scope) = function
	| SDecl (typ, name) ->
		let llvm_type = ltype_of_typ typ in
		let init_val = L.const_int llvm_type 0 in
		let llvm_val = L.define_global name init_val the_module in
		let scope = add_variable name (llvm_val, llvm_type) scope in
		(builder, scope)
	| SDefn (typ, name, expr) ->
		let llvm_type = ltype_of_typ typ in
		let expr_val = gen_expr builder scope expr in
		let llvm_val = L.build_alloca llvm_type name builder in
		let scope = add_variable name (llvm_val, llvm_type) scope in
		ignore (L.build_store expr_val llvm_val builder);
		(builder, scope)

	and gen_expr builder scope = function
	| (_, SUnaryOp (a, b)) ->  raise (Failure "UnaryOp not implemented yet")
	| (_, SLiteral i) -> L.const_int i32_t i
	| (_, SBoolLit b) -> L.const_int i1_t (if b then 1 else 0)
	| (_, SStrLit s) -> L.build_global_stringptr s "str" builder
	| (_, SChrLit c) -> raise (Failure "SChrLit not implemented yet")
	| (_, SFloatLit f) -> L.const_float float_t f
	| (_, SId id) ->
		let val_ptr = fst(lookup id scope) in
		L.build_load val_ptr (id ^ "_load") builder
	| (_, SBinop (a, op, b)) ->
		let e1 = gen_expr builder scope a
			and e2 = gen_expr builder scope b in
			(begin match op with
					 A.Add     -> L.build_add
				 | A.Sub     -> L.build_sub
				 | A.And     -> L.build_and
				 | A.Or      -> L.build_or
				 | A.Equal   -> L.build_icmp L.Icmp.Eq
				 | A.Neq     -> L.build_icmp L.Icmp.Ne
				 | A.Less    -> L.build_icmp L.Icmp.Slt
				 | A.Greater -> L.build_icmp L.Icmp.Sgt
				 | A.Gequal   -> L.build_icmp L.Icmp.Sge
				 | A.Lequal      -> L.build_icmp L.Icmp.Sle
				 | _        -> raise (Failure "Unimplemented binop")
			end ) e1 e2 "tmp" builder
	| (_, (SAssign (lhs, e))) ->
      let e' = gen_expr builder scope e in
      (begin
        match lhs with
        (_, SId id) -> ignore(L.build_store e' (fst(lookup id scope)) builder); e'
        | (_, SIndex (lst, i)) ->
          let lst' = gen_expr builder scope lst in 
          let l = gen_expr builder scope i in 
          let i' = L.build_fptosi l i32_t "i" builder in 
          let x = L.build_in_bounds_gep lst' [| i' |] "x" builder in 
          ignore (L.build_store e' x builder); e'
        | _ -> raise (Failure "something went wrong")
        end)
	(* | (_, SAssign ((_, SIndex(SId id, i)), e)) ->  *)
	(*      let e' = gen_expr builder scope e in *)
	(*      ignore(L.build_store e' (fst(lookup id scope)) builder); e' *)
	(* | (_, SListLit (a, b)) -> raise (Failure "SListLit not implemented yet")  *)
	| (_, SListLit (typ, lst)) -> 
      let n = L.const_int i32_t (List.length lst) in 
      let ty = ltype_of_typ typ in
      (* let vals = List.map () in  *)
      let arr = L.build_array_malloc ty n "list" builder in 
      let gen_list_item = 
        fun i x ->  let x' = gen_expr builder scope x in 
                    let i' = L.build_in_bounds_gep arr [|L.const_int i32_t i |] "i" builder in 
                    ignore (L.build_store x' i' builder) in
    List.iteri gen_list_item lst; arr
  | (_, SListComp(size, (typ, v))) ->
    (* let v' = gen_expr builder scope (typ, v) in *)
    let n = gen_expr builder scope size in
    let n' = L.build_fptosi n i32_t "def" builder in
    L.build_array_malloc (ltype_of_typ typ) n' "list" builder
  | (_, SIndex (lst, i)) -> 
      let lst' = gen_expr builder scope lst in 
      let l = gen_expr builder scope i in 
      let i' = L.build_fptosi l i32_t "i" builder in 
      let x = L.build_in_bounds_gep lst' [| i' |] "x" builder in 
      L.build_load x "val" builder
  | (A.FunSig(argtyps, rtyp), SFunction (binds, body)) -> 
		let name = (anon_name 10)
			and formal_types =
				Array.of_list (List.map (fun (t) -> ltype_of_typ t) argtyps) in
			let ret_type = ltype_of_typ rtyp in
			let ftype = get_llvm_function_type context the_module scope_struct_type formal_types ret_type in
		(* in let ftype = L.function_type (ltype_of_typ rtyp) formal_types in *)
			let fdef = L.define_function name ftype the_module in
			let builder = L.builder_at_end context (L.entry_block fdef) in
			(* let (builder, scope) = List.fold_left gen_bind (builder, scope) binds in *)

			(* Allocate and initialize scope variable as the first formal parameter *)
			let scope_formal = L.param fdef 0
			in
			L.set_value_name "scope" scope_formal;
			let scope_alloca = L.build_alloca (L.pointer_type scope_struct_type) "scope" builder
			in
			ignore (L.build_store scope_formal scope_alloca builder);

			gen_outer_scope_vars scope scope_formal builder;

			let local_vars =
				let add_formal m bind p = 
					let (t, n) = begin match bind with
						SDecl(ty, s) -> (ty, s) | SDefn(ty, s, _) -> (ty, s) end in
					let llvm_type = ltype_of_typ t in
					let local = L.build_alloca llvm_type n builder in
					ignore (L.build_store p local builder);
					add_variable n (local, llvm_type) m

				in
				let formals = List.fold_left2 add_formal scope binds
						(Array.to_list (Array.sub (L.params fdef) 1 (Array.length formal_types))) in
				formals
			in

			ignore(List.fold_left gen_stmt (builder, local_vars) body);
			fdef
	| (s, SCall((_,SId("print_str")), [e])) ->
			L.build_call printf_func [| str_format_str ; (gen_expr builder scope e) |]
				"printf" builder
	| (s, SCall((_,SId("print_int")), [e])) ->
		L.build_call printf_func [| int_format_str ; (gen_expr builder scope e) |]
			"printf" builder
	| (s, SCall((_,SId("print_float")), [e])) ->
		L.build_call printf_func [| float_format_str ; (gen_expr builder scope e) |]
			"printf" builder
	| (s, SCall((_,SId(name)), args)) ->

		(* print_endline("SCall"); *)
		let fdef_ptr = fst(lookup name scope) in
		(* print_endline("fdef_ptr: " ^ L.string_of_llvalue fdef_ptr); *)
		let fdef = L.build_load fdef_ptr "fdef" builder in
		(* print_endline("fdef: " ^ L.string_of_llvalue fdef); *)
		(* Generate LLVM expressions for arguments *)
		let llargs = List.map (gen_expr builder scope) args in
		(* print_endline("llargs done"); *)
		(* Include the current scope as the first argument *)
		let scope_val = create_llvm_scope context builder scope scope_struct_type in
		(* print_endline("scope_val done");	 *)
		let llargs_with_scope = scope_val :: llargs in
		let result = name ^ "_result" in
		let scope = create_scope ~parent:scope ()
 in
		let call_val = L.build_call fdef (Array.of_list llargs_with_scope) result builder in
		ignore(finish_scope scope);
		call_val
	(* | (s, SCall((_,SFunction(args, body)), [e])) -> *)
	| (s, SCall(anon_func, args)) ->
				let func_defn = gen_expr builder scope anon_func in
				let scope_val = create_llvm_scope context builder scope scope_struct_type in
				let llargs = List.map (gen_expr builder scope) args in
				let llargs_with_scope = scope_val :: llargs in
				let result = (anon_name 10) ^ "_result" in
				L.build_call func_defn (Array.of_list llargs_with_scope) result builder
	| (_, SStructId s) -> raise (Failure "SStructId not implemented yet")
	 | (_, SStructAccess s) -> raise (Failure "SStructAccess not implemented yet")
	| (_, SStructAssign s) -> raise (Failure "SStructAssign not implemented yet") 
	| _ -> raise (Failure ("gen_expr match not found"))
	(* Handle other cases as needed *)

and gen_if_stmt builder scope cond then_stmt else_stmt_opt =
	let then_bb = L.append_block context "then" the_function in
	let else_bb = L.append_block context "else" the_function in
	let merge_bb = L.append_block context "merge" the_function in

	ignore (L.build_cond_br (gen_expr builder scope cond) then_bb else_bb builder);

	(* Generate code for the "then" branch *)
	L.position_at_end then_bb builder;
	ignore (gen_stmt (builder, scope) then_stmt);
	ignore (L.insertion_block builder);
	ignore (L.build_br merge_bb builder); (* Jump to the merge block *)

	(* Generate code for the "else" branch *)
L.position_at_end else_bb builder;
	(match else_stmt_opt with
	| Some else_stmt -> ignore (gen_stmt (builder, scope) else_stmt)
	| None -> ());
	ignore (L.insertion_block builder);
	ignore (L.build_br merge_bb builder); (* Jump to the merge block *)

	(* Move to the merge block *)
	L.position_at_end merge_bb builder

and gen_while_stmt builder scope cond body =
	let cond_bb = L.append_block context "while_cond" the_function in
	let body_bb = L.append_block context "while_body" the_function in
	let merge_bb = L.append_block context "merge" the_function in
	ignore (L.build_br cond_bb builder);
	let cond_builder = L.builder_at_end context cond_bb in
	let cond_val = gen_expr cond_builder scope cond in
	ignore (L.build_cond_br cond_val body_bb merge_bb cond_builder);
	let body_builder = L.builder_at_end context body_bb in
	let (body_builder, _) = gen_stmt (body_builder, scope) body in
	ignore (L.build_br cond_bb body_builder);
	L.position_at_end merge_bb builder

and gen_for_stmt builder scope init cond step body = 
	let init_bb = L.append_block context "for_init" the_function in
	let cond_bb = L.append_block context "for_cond" the_function in
	let body_bb = L.append_block context "for_body" the_function in
	let inc_bb = L.append_block context "for_inc" the_function in
	let merge_bb = L.append_block context "merge" the_function in
	ignore (L.build_br init_bb builder);

	let init_builder = L.builder_at_end context init_bb in
	let (_, new_scope) = gen_bind (init_builder, scope) init in
	ignore(L.build_br cond_bb init_builder);
	
	let cond_builder = L.builder_at_end context cond_bb in
	let cond_val = gen_expr cond_builder new_scope cond in
	ignore (L.build_cond_br cond_val body_bb merge_bb cond_builder);
	
	let body_builder = L.builder_at_end context body_bb in
	let (body_builder, _) = gen_stmt (body_builder, new_scope) body in
	ignore (L.build_br inc_bb body_builder);

	let inc_builder = L.builder_at_end context inc_bb in
	ignore (gen_expr inc_builder new_scope step);
	ignore (L.build_br cond_bb inc_builder);
	L.position_at_end merge_bb builder


and gen_return builder scope expr =
	let ret_val = gen_expr builder scope expr in
	ignore (L.build_ret ret_val builder) in

	(* List.iter (gen_stmt builder) sbody; *)
	let builder = L.builder_at_end context (L.entry_block main_func) in
	let (final_builder, scope) = List.fold_left gen_stmt (builder, global_scope) sbody in

	ignore(L.build_ret (L.const_int i32_t 0) final_builder);
	the_module

