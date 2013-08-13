(* Problem 54:
 * Write a predicate isTree which returns true if and only if its argument is
 * a list representing a binary tree.
 *
 * Because OCaml is strongly typed, this problem is pointless. 
 *)

type 'a binary_tree =
  | Nil
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;
