(* Problem 42:
 * Truth tables for logical expressions (2 variables).
 *
 * Define a function, table2, which returns the truth table of a given logical
 * expression in two variables (specified as arguments). The return value must
 * be a list of triples containing
 * (value_of_a, value_of_b, value_of_expr).
 *)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or  of bool_expr * bool_expr

exception InvalidInput

let rec eval2 (a : string) (val_a : bool) (b : string) (val_b : bool) 
(expr : bool_expr) : bool =
  match expr with
  | Var x ->
    if x = a then val_a
    else if x = b then val_b
    else raise InvalidInput
  | Not e -> not (eval2 a val_a b val_b e)
  | And (e1, e2) -> (eval2 a val_a b val_b e1) && (eval2 a val_a b val_b e2)
  | Or (e1, e2) -> (eval2 a val_a b val_b e1) || (eval2 a val_a b val_b e2)

let table2 (a : string) (b : string) (expr : bool_expr) 
: (bool * bool * bool) list =
  [ (true, true, eval2 a true b true expr);
    (true, false, eval2 a true b false expr);
    (false, true, eval2 a false b true expr);
    (false, false, eval2 a false b false expr) ]
;;
