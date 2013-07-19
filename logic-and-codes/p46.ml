(* The definition of the "small" language for boolean expressions used in
 * this section. *)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or  of bool_expr * bool_expr
;;

(* A logic expression in two variables can then be written in prefix
 * notation, as in the following example. *)
And(Or(Var "a", Var "b"), And(Var "a", Var "b"));;
