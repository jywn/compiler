open Program
open Ir

type rdef = instr

module RDSet = Set.Make(struct
  type t = rdef
  let compare = Pervasives.compare
end)

let get_defined_vars instr =
  match instr with
  | Set (reg, _) -> [reg]
  | _ -> []

let propagate_constants instrs =
  let in_sets = ref RDSet.empty in
  let out_sets = ref RDSet.empty in
  let var_values = ref (StrMap.empty : value StrMap.t) in

  let rec compute_in_out instrs =
    match instrs with
    | [] -> ()
    | instr :: tail ->
      let defined_vars = get_defined_vars instr in
      out_sets := RDSet.diff !out_sets (RDSet.of_list defined_vars);
      in_sets := RDSet.add instr (RDSet.union !in_sets !out_sets);

      (* Update var_values based on reaching definitions *)
      List.iter (fun var -> var_values := StrMap.remove var !var_values) defined_vars;
      List.iter (fun var -> var_values := StrMap.add var instr !var_values) defined_vars;

      compute_in_out tail;
      out_sets := RDSet.add instr !out_sets;
  in

  let rec propagate_rec instrs =
    match instrs with
    | [] -> []
    | instr :: tail ->
      let updated_instr =
        match instr with
        | Set (reg, _) when StrMap.mem reg !var_values ->
          (* Replace with the propagated constant value from reaching definitions *)
          let propagated_value = StrMap.find reg !var_values in
          Set (reg, propagated_value)
        | _ -> instr
      in
      updated_instr :: propagate_rec tail
  in

  compute_in_out instrs;
  propagate_rec instrs

let run (ir: ir_code): ir_code =
  let (f, args, instrs) = ir in
  let propagated = propagate_constants instrs in
  (f, args, propagated)