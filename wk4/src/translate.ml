open Program
open Ir
open Helper

(* The module for symbol table. Carefully think about what should be stored in
 * the symbol table for this IR translation phase. *)
module SymbolMap = Map.Make(String)
(*variable_name, type, address*)

(* Let's assume that boolean is 1-byte and integer is 4-byte. *)
let sizeof ctyp =
  match ctyp with
  | CInt -> 4
  | CBool -> 1
  | CIntArr n -> 4 * n
  | CBoolArr n -> n

let rec extract_names args =
  match args with
  | [] -> []
  | (arg_typ, arg_name) :: tail_args -> arg_name :: extract_names tail_args

(*Set a policy for each expression*)
let rec trans_exp sym_tab e = (*return ([instr list], result register)*)
  match e with 
  | ConstBool b ->  
    let r = create_register_name () in
    (Set (r, ImmBool b) :: [], r)
    (*return register has value*)
  | ConstInt i ->
    let r = create_register_name () in
    (Set (r, ImmInt i) :: [],  r)
    (*return register has value*)
  | Var vname ->
    let r = create_register_name () in
    let (v_type, v_reg) = SymbolMap.find vname sym_tab in
    (Load (r, v_reg) :: [], r) 
    (*return register has value*)
  | Arr (vname, index) ->  (*index가 이미 존재하는 local var이라면?*)
    let (v_type, v_reg) = SymbolMap.find vname sym_tab in
    let (instrs, reg) = trans_exp sym_tab index in
    let r = create_register_name () in
    (match v_type with
    | CIntArr n ->
      (instrs @ BinOp (r, MulOp, Imm (ImmInt 4), Reg reg) :: BinOp (r, AddOp, Reg v_reg, Reg r) :: Load (r, r) :: [], r)
    | CBoolArr n ->
      (instrs @ BinOp (r, MulOp, Imm (ImmInt 1), Reg reg) :: BinOp (r, AddOp, Reg v_reg, Reg r) :: Load (r, r) :: [], r)
    | _ -> ([], r)
    )(*return register has value*)
  | Add (e1, e2) -> 
    let r = create_register_name () in
    let (instrs_1, reg_1) = trans_exp sym_tab e1 in
    let (instrs_2, reg_2) = trans_exp sym_tab e2 in
    (instrs_1 @ instrs_2 @ BinOp (r, AddOp, Reg reg_1, Reg reg_2) :: [], r)
    (*return register has value*)
  | Sub (e1, e2) ->
    let r = create_register_name () in
    let (instrs_1, reg_1) = trans_exp sym_tab e1 in
    let (instrs_2, reg_2) = trans_exp sym_tab e2 in
    (instrs_1 @ instrs_2 @ BinOp (r, SubOp, Reg reg_1, Reg reg_2) :: [], r)
  | Mul (e1, e2) ->
    let r = create_register_name () in
    let (instrs_1, reg_1) = trans_exp sym_tab e1 in
    let (instrs_2, reg_2) = trans_exp sym_tab e2 in
    (instrs_1 @ instrs_2 @ BinOp (r, MulOp, Reg reg_1, Reg reg_2) :: [], r)
  | Div (e1, e2) ->
    let r = create_register_name () in
    let (instrs_1, reg_1) = trans_exp sym_tab e1 in
    let (instrs_2, reg_2) = trans_exp sym_tab e2 in
    (instrs_1 @ instrs_2 @ BinOp (r, DivOp, Reg reg_1, Reg reg_2) :: [], r)
  | Neg (e1) ->
    let r = create_register_name () in
    let (instrs_1, reg_1) = trans_exp sym_tab e1 in
    (instrs_1 @ UnOp (r, NegOp, Reg reg_1) :: [], r)
  | Equal (e1, e2) ->
    let r = create_register_name () in
    let (instrs_1, reg_1) = trans_exp sym_tab e1 in
    let (instrs_2, reg_2) = trans_exp sym_tab e2 in
    (instrs_1 @ instrs_2 @ BinOp (r, EqOp, Reg reg_1, Reg reg_2) :: [], r)
  | NotEq (e1, e2) ->
    let r = create_register_name () in
    let (instrs_1, reg_1) = trans_exp sym_tab e1 in
    let (instrs_2, reg_2) = trans_exp sym_tab e2 in
    (instrs_1 @ instrs_2 @ BinOp (r, NeqOp, Reg reg_1, Reg reg_2) :: [], r)
  | LessEq (e1, e2) ->
    let r = create_register_name () in
    let (instrs_1, reg_1) = trans_exp sym_tab e1 in
    let (instrs_2, reg_2) = trans_exp sym_tab e2 in
    (instrs_1 @ instrs_2 @ BinOp (r, LeqOp, Reg reg_1, Reg reg_2) :: [], r)
  | LessThan (e1, e2) ->
    let r = create_register_name () in
    let (instrs_1, reg_1) = trans_exp sym_tab e1 in
    let (instrs_2, reg_2) = trans_exp sym_tab e2 in
    (instrs_1 @ instrs_2 @ BinOp (r, LtOp, Reg reg_1, Reg reg_2) :: [], r)
  | GreaterEq (e1, e2) ->
    let r = create_register_name () in
    let (instrs_1, reg_1) = trans_exp sym_tab e1 in
    let (instrs_2, reg_2) = trans_exp sym_tab e2 in
    (instrs_1 @ instrs_2 @ BinOp (r, GeqOp, Reg reg_1, Reg reg_2) :: [], r)
  | GreaterThan (e1, e2) ->
    let r = create_register_name () in
    let (instrs_1, reg_1) = trans_exp sym_tab e1 in
    let (instrs_2, reg_2) = trans_exp sym_tab e2 in
    (instrs_1 @ instrs_2 @ BinOp (r, GtOp, Reg reg_1, Reg reg_2) :: [], r)
    (*return register has value*)
  | And (e1, e2) ->    
    let (instrs_1, t_1) = trans_exp sym_tab e1 in
    let (instrs_2, t_2) = trans_exp sym_tab e2 in
    let t_3 = create_register_name () in
    let t_4 = create_register_name () in
    let l_1 = create_label () in
    let l_2 = create_label () in
    (instrs_1 @ [GotoIfNot (t_1, l_1)] @ instrs_2 @ [Copy (t_2, t_3)] @ [Goto (l_2)] @ [Label (l_1)] @ [Set (t_3, ImmBool false)] @ [Label (l_2)] @ [Copy (t_3, t_4)], t_4)
    (*return register has value*)
  | Or (e1, e2) ->
    let (instrs_1, t_1) = trans_exp sym_tab e1 in
    let (instrs_2, t_2) = trans_exp sym_tab e2 in
    let t_3 = create_register_name () in
    let t_4 = create_register_name () in
    let l_1 = create_label () in
    let l_2 = create_label () in
    (instrs_1 @ [GotoIf (t_1, l_1)] @ instrs_2 @ [Copy (t_2, t_3)] @ [Goto (l_2)] @ [Label (l_1)] @ [Set (t_3, ImmBool true)] @ [Label (l_2)] @ [Copy (t_3, t_4)], t_4)
  | Not (e1) ->
    let r = create_register_name () in
    let (instrs_1, reg_1) = trans_exp sym_tab e1 in
    (instrs_1 @ UnOp (r, NotOp, Reg reg_1) :: [], r)


let rec trans_stmt sym_tab stmts =
  match stmts with
  | [] -> []
  | head :: tail ->
    match head with
    | LocalDecl decl ->
      let (vtype, vname) = decl in
      let vsize = sizeof vtype in
      let vreg = create_register_name () in
      let sym_tab = SymbolMap.add vname (vtype, vreg) sym_tab in
      (*vreg: register has address*)
      ([LocalAlloc (vreg, vsize)]) @ trans_stmt sym_tab tail
    | Assign (lvalue, exp) ->
      (match lvalue with
      | LVar l_vname ->
        let (vtype, vreg) = SymbolMap.find l_vname sym_tab in
        let (instrs, reg) = trans_exp sym_tab exp in
        instrs @ ([Store (Reg reg, vreg)]) @ trans_stmt sym_tab tail
      | LArr (l_vname, l_idx) ->
        let (vtype, vreg) = SymbolMap.find l_vname sym_tab in
        let r = create_register_name () in
        let (instr, reg) = trans_exp sym_tab l_idx in
        let (instrs_1, reg_1) = trans_exp sym_tab exp in
        (match vtype with
        | CIntArr n ->
          instr @ [BinOp (r, MulOp, Imm (ImmInt 4), Reg reg)] @ instrs_1 @ [BinOp (r, AddOp, Reg vreg, Reg r)] @ [Store (Reg reg_1, r)] @ trans_stmt sym_tab tail
        | CBoolArr n ->
          instr @ [BinOp (r, MulOp, Imm (ImmInt 1), Reg reg)] @ instrs_1 @ [BinOp (r, AddOp, Reg vreg, Reg r)] @ [Store (Reg reg_1, r)] @ trans_stmt sym_tab tail
        | _ -> trans_stmt sym_tab tail
        )
      )
    | ReturnValue (exp) ->
      let (instr, reg) = trans_exp sym_tab exp in
      instr @ [Ret (Reg reg)] @ trans_stmt sym_tab tail
    | If (exp, s1_list, s2_list) ->      
      let label_body = create_label () in
      let label_fin = create_label () in
      let (instrs, reg) = trans_exp sym_tab exp in
      instrs @ [GotoIfNot (reg, label_body)] @ trans_stmt sym_tab s1_list @ [Goto (label_fin)] @ [Label (label_body)] @ trans_stmt sym_tab s2_list @ [Label (label_fin)] @ trans_stmt sym_tab tail  
    | While (exp, s1_list) ->
      let label_cond = create_label () in
      let label_body = create_label () in
      let label_fin = create_label () in
      let (instrs, reg) = trans_exp sym_tab exp in 
      [Label (label_cond)] @ instrs @ [GotoIf (reg, label_body)] @ [Goto (label_fin)] @ [Label (label_body)] @ trans_stmt sym_tab s1_list @ [Goto (label_cond)] @ [Label (label_fin)] @ trans_stmt sym_tab tail


let rec upload_args_reg sym_tab arg_regs = (*instruction list, symbol table*)
  match arg_regs with
  | [] -> []
  | head :: tail ->
    let (vtype, vreg) = SymbolMap.find head sym_tab in
    let r = create_register_name () in
    let vsize = sizeof vtype in
    [Copy (vreg, r)] @ [LocalAlloc (vreg, vsize)] @ [Store (Reg r, vreg)] @ upload_args_reg sym_tab tail

let rec collect_args sym_tab args arg_regs = 
  match args, arg_regs with
  | [], [] -> sym_tab
  | _::_, [] -> sym_tab
	| [], _::_ -> sym_tab
  | h1 :: t1, h2 :: t2 ->
    let (vtype, vname) = h1 in
    let (arg_reg) = h2 in
    let sym_tab = SymbolMap.add vname (vtype, arg_reg) sym_tab in
    collect_args sym_tab t1 t2

let run (p: program): ir_code =
  let (fname, ret_type, args, stmts) = p in
  let arg_regs = extract_names args in
  let sym_tab = collect_args SymbolMap.empty args arg_regs in
  let instrs_1 = upload_args_reg sym_tab arg_regs in
  let instrs_2 = trans_stmt sym_tab stmts in
  let instrs = instrs_1 @ instrs_2 in
  (* Example code for generating IR instructions. *)
  (*
  let r = create_register_name () in (* Defined in helper.ml *)
  let set_instr = Set (r, ImmInt 0) in
  let ret_instr = Ret (Reg r) in
  let instrs = [set_instr; ret_instr] in
  *)
  (fname, arg_regs, instrs)
