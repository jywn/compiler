(* DO NOT change the definition of this type *)
type exp =
    Num of int
  | Add of exp * exp
  | Sub of exp * exp
  | Mul of exp * exp
  | Div of exp * exp

(* Return the integer value represented by the expression 'e'. *)
let rec eval e =
  match e with
  | Num i -> i
  | Add (e1, e2) -> e1 + e2
  | Sub (e1, e2) -> e1 - e2
  | Mul (e1, e2) -> e1 * e2
  | Div (e1, e2) -> e1 / e2
