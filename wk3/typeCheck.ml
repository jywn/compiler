open Program
open Error

(* Our symbol table will be a mapping from 'string' to 'ctype_entry'. *)
type ctype_entry =
  | VarType of ctype
  | FuncType of ctype * ctype list (* return type * argument type list *)

(* Define a module for symbol table *)
module SymbolMap = Map.Make(String)

(* During the semantic analysis, this type can be helpful. Why do we need this
 * even though 'ctype' is already defined? If you encounter a wrong expression
 * during the semantic analysis (for example "1 + true"), you cannot decide its
 * type but still may have to return something. *)
type typ = Void | Int | Bool | Unknown

let ctyp_to_typ ctyp =
  match ctyp with
  | CVoid -> Void
  | CInt -> Int
  | CBool -> Bool

let typ_to_ctyp ctyp =
  match ctyp with
  | Void -> CVoid
  | Int -> CInt
  | Bool -> CBool
  | Unknown -> (* Raise exception *)
      failwith "Not allowed (You should not call this in such situation)"

(*~~TO DO*)
(*split args?*)
(*let rec split_args args =
  match args with
  | [] -> []
	| (CVoid) :: rest -> CVoid :: split_args rest
  | (ctype) :: rest -> 
		(match rest with
		| [] -> ctype :: split_args []
		| (vname) :: tail -> ctype :: split_args tail
		)*)
(*let rec split_args args =
	match args with
	| [] -> []
	| ctype, aname :: tail ->
		ctype :: split_args tail
	| ctype, [] :: ->
		ctype :: split_args tail
	| ctype, _ :: ->
		ctype :: split_args tail*)
let rec split_args args =
	match args with
	| [] -> []
	| (ctype, _) :: tail -> ctype :: split_args tail
(* Record the type of variables into the symbol table *)
let rec collect_vars decls sym_tab =
  match decls with
  | [] -> sym_tab
  | head_decl :: tail_decls ->
      let (ctyp, vname) = head_decl in
      let sym_tab = SymbolMap.add vname (VarType ctyp) sym_tab in
      collect_vars tail_decls sym_tab

(* Record the type of functions into the symbol table *)
let rec collect_functions funcs sym_tab =
  match funcs with
  | [] -> sym_tab
  | head_decl :: tail_decls ->
		let (fname, ret_typ, args, stmts) = head_decl in
		let filtered = split_args args in
    let sym_tab = SymbolMap.add fname (FuncType (ret_typ, filtered)) sym_tab in
		collect_functions tail_decls sym_tab
	
(* Check expression 'e' and return detected semantic errors, along with the
 * decided type of 'e'. If the type of expression cannot be decided due to
 * semantic error, return 'Unknown' as its type. *)
let rec check_exp sym_tab e =
  match e with
  | ConstBool b -> ([], Bool)
  | ConstInt i -> ([], Int)
  | Var vname ->
    (match SymbolMap.find vname sym_tab with
    | exception Not_found -> ([UndefinedName vname], Unknown)
    | VarType ctyp -> ([], ctyp_to_typ ctyp)
		| _ -> ([], Unknown)
		)
	| Add (e1, e2) ->
  	let (p1, p2) = check_exp sym_tab e1 in
  	let (q1, q2) = check_exp sym_tab e2 in
  	(match (p1, p2), (q1, q2) with
  	| (([], Int), ([], Int)) -> ([], Int)
  	| _ -> ([OperandMismatch], Unknown))
	| Sub (e1, e2) ->
  	let (p1, p2) = check_exp sym_tab e1 in
  	let (q1, q2) = check_exp sym_tab e2 in
  	(match (p1, p2), (q1, q2) with
  	| (([], Int), ([], Int)) -> ([], Int)
  	| _ -> ([OperandMismatch], Unknown))
	| Mul (e1, e2) ->
  	let (p1, p2) = check_exp sym_tab e1 in
  	let (q1, q2) = check_exp sym_tab e2 in
  	(match (p1, p2), (q1, q2) with
  	| (([], Int), ([], Int)) -> ([], Int)
  	| _ -> ([OperandMismatch], Unknown))
	| Div (e1, e2) ->
  	let (p1, p2) = check_exp sym_tab e1 in
  	let (q1, q2) = check_exp sym_tab e2 in
  	(match (p1, p2), (q1, q2) with
  	| (([], Int), ([], Int)) -> ([], Int)
  	| _ -> ([OperandMismatch], Unknown))
  | Neg e1 ->
		let (p1, p2) = check_exp sym_tab e1 in
		(match (p1, p2) with
		| ([], Int) -> ([], Int)
		| _ -> ([OperandMismatch], Unknown))
  | Equal (e1, e2) ->
		let (p1, p2) = check_exp sym_tab e1 in
		let (q1, q2) = check_exp sym_tab e2 in
		(match (p1, p2), (q1, q2) with
		| (([], Int), ([], Int)) -> ([], Bool)
		| (([], Bool), ([], Bool)) -> ([], Bool)
		| _ -> ([OperandMismatch], Unknown))
  | NotEq (e1, e2) ->
		let (p1, p2) = check_exp sym_tab e1 in
		let (q1, q2) = check_exp sym_tab e2 in
		(match (p1, p2), (q1, q2) with
		| (([], Int), ([], Int)) -> ([], Bool)
		| (([], Bool), ([], Bool)) -> ([], Bool)
		| _ -> ([OperandMismatch], Unknown))
  | LessEq (e1, e2) ->
		let (p1, p2) = check_exp sym_tab e1 in
		let (q1, q2) = check_exp sym_tab e2 in
		(match (p1, p2), (q1, q2) with
		| (([], Int), ([], Int)) -> ([], Bool)
		| _ -> ([OperandMismatch], Unknown))
  | LessThan (e1, e2) ->
		let (p1, p2) = check_exp sym_tab e1 in
		let (q1, q2) = check_exp sym_tab e2 in
		(match (p1, p2), (q1, q2) with
		| (([], Int), ([], Int)) -> ([], Bool)
		| _ -> ([OperandMismatch], Unknown))
  | GreaterEq (e1, e2) ->
		let (p1, p2) = check_exp sym_tab e1 in
		let (q1, q2) = check_exp sym_tab e2 in
		(match (p1, p2), (q1, q2) with
		| (([], Int), ([], Int)) -> ([], Bool)
		| _ -> ([OperandMismatch], Unknown))
  | GreaterThan (e1, e2) ->
		let (p1, p2) = check_exp sym_tab e1 in
		let (q1, q2) = check_exp sym_tab e2 in
		(match (p1, p2), (q1, q2) with
		| (([], Int), ([], Int)) -> ([], Bool)
		| _ -> ([OperandMismatch], Unknown))
  | And (e1, e2) ->
		let (p1, p2) = check_exp sym_tab e1 in
		let (q1, q2) = check_exp sym_tab e2 in
		(match (p1, p2), (q1, q2) with
		| (([], Bool), ([], Bool)) -> ([], Bool)
		| _ -> ([OperandMismatch], Unknown))
  | Or (e1, e2) ->
		let (p1, p2) = check_exp sym_tab e1 in
		let (q1, q2) = check_exp sym_tab e2 in
		(match (p1, p2), (q1, q2) with
		| (([], Bool), ([], Bool)) -> ([], Bool)
		| _ -> ([OperandMismatch], Unknown))
  | Not e1 ->
		let (p1, p2) = check_exp sym_tab e1 in
		(match (p1, p2) with
		| ([], Bool)-> ([], Bool)
		| _ -> ([OperandMismatch], Unknown))
  | CallExp (fname, exps) -> (*LEFT*)
		(match SymbolMap.find fname sym_tab with 
		| exception Not_found -> ([UndefinedName fname], Unknown)
		| VarType vtyp -> ([CallingVariable fname], Unknown)
		| FuncType(ret_typ, args) -> 
			let result_args = check_args_2 sym_tab exps args in
			(match result_args with
			| [] -> ([], ctyp_to_typ ret_typ)
			| _ -> (result_args, Unknown)
			)
		)
  (* TODO: Fill in the remaining cases below *)
  | _ -> ([], Unknown) (*LEFT*)

(******************************************************************
 * And of course, you will need many more functions between here. *
 * ****************************************************************)

(* Check functions and return detected semantic errors. *)
(*undefinedName assignmiss returnmiss callingmiss argtype argnum *)
(*
let rec check_args sym_tab args args_ = (*args: 4, true, ... args_: int, void, ... *)
  match args, args_ with
  | ([], []) -> ([])
  | ([], _::_) -> ([ArgNumMismatch])
  | (_::_, []) -> ([ArgNumMismatch]) (*not here, at stmts*)
  | h :: t, h_ :: t_ ->
		let (cond1, cond2) = check_exp sym_tab h in
		let (cond1_, cond2_) = check_exp sym_tab h_ in
		(match (cond1, cond2), (cond1_, cond2_) with
		| (cond1, cond2), (cond1_, cond2_) when  cond2 <> cond2_ -> ([ArgTypeMismatch (typ_to_ctyp cond2, typ_to_ctyp cond2_)])
		| _ -> check_args sym_tab t t_
		)
*)
and check_args_2 sym_tab exp_value_args ctyp_args =
	match exp_value_args, ctyp_args with
  | ([], []) -> ([])
  | ([], _::_) -> ([ArgNumMismatch])
	| (_::_, []) -> ([ArgNumMismatch])
  | h :: t, h_ :: t_ ->
		let (p, q) = check_exp sym_tab h in
		(try	let r = typ_to_ctyp q in
			(match r, h_ with
			| r, h_ when r <> h_ -> ([ArgTypeMismatch (h_, r)])
			| r, h_ when h_ == CVoid -> check_args_2 sym_tab exp_value_args t_
			| _ -> check_args_2 sym_tab t t_
			) 
		with
		| Failure msg -> ([OperandMismatch])
		| _ -> check_args_2 sym_tab t t_
		)




let rec check_stmts sym_tab fname stmts =
  match stmts with
  | [] -> ([])
  | head :: tail ->
		match head with
		| LocalDecl (decl) -> 
			let (ctyp, vname) = decl in
			let local_sym_tab = SymbolMap.add vname (VarType ctyp) sym_tab in
			check_stmts local_sym_tab fname tail
		| Assign (name, exp) ->
			(match SymbolMap.find name sym_tab with
			| exception Not_found -> ([UndefinedName name]) @ check_stmts sym_tab fname tail
		  | (VarType ctyp) -> 
				let (q1, q2) = check_exp sym_tab exp in
				(match (q1, q2) with
				| (p, Unknown) -> (p) @ check_stmts sym_tab fname tail
				| ([], q2) when ctyp <> (typ_to_ctyp q2) -> ([AssignMismatch (ctyp, typ_to_ctyp q2)]) @ check_stmts sym_tab fname tail
				| _ -> check_stmts sym_tab fname tail
				)
			| (FuncType (ret_typ, args)) ->	([UsingFunctionAsVar name]) @ check_stmts sym_tab fname tail
			)
		| Call (str, exps) ->
			(match SymbolMap.find str sym_tab with
			| exception Not_found -> ([UndefinedName str]) @ check_stmts sym_tab fname tail
			| VarType vtyp -> ([CallingVariable str]) @ check_stmts sym_tab fname tail
			| (FuncType (ret_typ, args)) -> (*argnum, type*) (*args from map. int, void, ...*)
				let result_args = check_args_2 sym_tab exps args in
				result_args @ check_stmts sym_tab fname tail
			)
		| Return ->
				(match SymbolMap.find fname sym_tab with
				| exception Not_found -> ([UndefinedName fname]) @ check_stmts sym_tab fname tail
				| VarType vtyp -> ([CallingVariable fname]) @ check_stmts sym_tab fname tail
				| (FuncType (ret_typ, args)) ->
					(match ret_typ with 
					| CVoid -> ([]) @ check_stmts sym_tab fname tail
					| CBool -> ([ReturnMismatch (CBool, CVoid)]) @ check_stmts sym_tab fname tail
					| CInt -> ([ReturnMismatch (CInt, CVoid)]) @ check_stmts sym_tab fname tail
					)
				)
		| ReturnValue (exp) ->
			(match SymbolMap.find fname sym_tab with
			| exception Not_found -> ([UndefinedName fname]) @ check_stmts sym_tab fname tail
			| VarType vtyp -> ([CallingVariable fname]) @ check_stmts sym_tab fname tail
			| (FuncType (ret_typ, args)) -> 
				let (p, q) = check_exp sym_tab exp in
				(match (p, q) with
				| (p, Unknown) -> (p) @ check_stmts sym_tab fname tail
				| (_, Void) when ret_typ <> CVoid -> ([ReturnMismatch (ret_typ, CVoid)]) @ check_stmts sym_tab fname tail
				| (_, Int) when ret_typ <> CInt -> ([ReturnMismatch (ret_typ, CInt)]) @ check_stmts sym_tab fname tail
				| (_, Bool) when ret_typ <> CBool -> ([ReturnMismatch (ret_typ, CBool)]) @ check_stmts sym_tab fname tail
				| _ -> ([]) @ check_stmts sym_tab fname tail
				)
			)
		| If (exp, stmt_1, stmt_2) ->
			let (p1, p2) = check_exp sym_tab exp in
			let q = check_stmts sym_tab fname stmt_1 in
			let r = check_stmts sym_tab fname stmt_2 in
			(match p2 with
			| Bool -> q @ r @ check_stmts sym_tab fname tail
			| _ -> ([OperandMismatch]) @ q @ r @ check_stmts sym_tab fname tail
			)
		| While (exp, stmt_list) ->
			let p = check_exp sym_tab exp in
			(match p with
			| ([], Bool) -> check_stmts sym_tab fname stmt_list @ check_stmts sym_tab fname tail
			| _ -> ([OperandMismatch]) @ check_stmts sym_tab fname stmt_list @ check_stmts sym_tab fname tail
			)
		

let rec update_argument sym_tab args =
	match args with
	| [] -> sym_tab
	| head :: tail ->
		let (vtyp, vname) = head in
		let sym_tab = 
		(match SymbolMap.find vname sym_tab with
		| exception Not_found -> SymbolMap.add vname (VarType vtyp) sym_tab
		| _ -> SymbolMap.add vname (VarType vtyp) sym_tab
		) in
		update_argument sym_tab tail
		(*check whether already exists, if it does, delete it and assign*)
		

let rec check_functions sym_tab funcs =
	match funcs with
	| [] -> []
	| (fname, ret_typ, args, stmts) :: tail ->
			(match SymbolMap.find fname sym_tab with
			| exception Not_found -> ([UndefinedName fname]) @ check_functions sym_tab tail 
			| VarType vtyp -> ([CallingVariable fname]) @ check_functions sym_tab tail 
			| (FuncType (ret_, args_)) ->
				let local_tab = update_argument sym_tab args in
				let result = check_stmts local_tab fname stmts in
				result @ check_functions sym_tab tail
			)
			(*1 option, 매 함수 확인마다 청소 후 collect funcs*)

(* Check a program and return detected semantic errors. *)
let run (p: program) : error list =
  let (gdecls, funcs) = p in
  let sym_tab = collect_vars gdecls SymbolMap.empty in
  let sym_tab = collect_functions funcs sym_tab in
  (* At this point, 'sym_tab' must contain global variables & functions *)
  check_functions sym_tab funcs
