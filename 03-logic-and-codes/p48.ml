(* Problem 43
 * Truth tables for logical expressions
 *
 * Generalize the previous problem in such a way that the logical expression
 * may contain any number of logical variables. Define #table# in a way that
 * #table variable expr# returns the truth table for the expression #expr#, 
 * which contains the logical variables enumerated in #variables#.
 *
 * val table : string list -> bool_expr -> (((string * bool) list list) * bool)
 *
 * TODO:
 * Implement a #constr_inputs# with the type:
 * val constr_inputs : string list -> (string * bool) list list
 *)

let rec eval (vals : (string * bool) list) (expr : bool_expr) : bool =
  match expr with
  | Var x -> List.assoc x vals
  | And (e1, e2) -> (eval vals e1) && (eval vals e2)
  | Or (e1, e2) -> (eval vals e1) || (eval vals e2)
  | Not e -> not (eval vals e)

let table (vars : string list) (expr : bool_expr) 
: ((string * bool) list * bool) list =
  let rec calculate (acc : (string * bool) list) (lst : string list) =
    match lst with
    | [] ->  [acc, eval acc expr]
    | h :: t ->
      (calculate ((h, true) :: acc) t)
      @ (calculate ((h, false) :: acc) t)
  in
  calculate [] vars
;;
