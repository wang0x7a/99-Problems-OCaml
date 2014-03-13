(* Find the lowest common ancestor of two nodes in a binary tree*)

open Core.Std

exception Not_found

type 'a binary_tree = 
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

(*
let rec lca root p q =
  match (root, p, q) with
  | (Empty, _, _) | (_, Empty, _) | (_, _, Empty) -> Empty
  | (Node (x, left, right), Node (y, _, _), Node (z, _, _)) ->
    if x = y || x = z then root
    else
      let left = lca left p q in
      let right = lca right p q in
      match left, right with
      | (Node (_, _, _), Node (_, _, _)) -> root
      | (Node (_, _, _), Empty) -> left
      | (Empty, Node (_, _, _)) -> right
      | (Empty, Empty) -> Empty
;;
*)

(* Only extrac the value of the ancestor *)
let lca root p q =
  let rec helper root p q =
    match (root, p, q) with
    | (Empty, _, _) | (_, Empty, _) | (_, _, Empty) -> Empty
    | (Node (x, left, right), Node (y, _, _), Node (z, _, _)) ->
        if x = y || x = z then root
        else
          let left = helper left p q in
          let right = helper right p q in
          match (left, right) with
          | (Node (_, _, _), Node (_, _, _)) -> root
          | (Node (_, _, _), Empty) -> left
          | (Empty, Node (_, _, _)) -> right
          | (Empty, Empty) -> Empty 
  in
  match (helper root p q) with
  | Emtpy -> raise Not_found
  | Node (x, _, _) -> x
;;
