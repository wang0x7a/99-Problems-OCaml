(* Problem:
   Create a list containing all integers within a given range.
   If the first argument is smaller than the second, 
   produce a list in decreasing order.

   Function Type:
   val range : int -> int -> int list
 *)

let range (p : int) (q : int) : int list =
  let s = min p q
  and e = max p q in
  
  let rec aux (acc : int list) (i : int) : int list =
    if i <= e then aux (i :: acc) (i + 1)
    else
      if p = s then List.rev acc
      else acc
  in aux [] s
;;

(* Implemenation by VictorNicollet *)
let range (p : int) (q : int) : int list =
  let rec aux (acc : int list) (a : int) (b : int) : int list =
    if a > b then []
    else if a = b then a :: acc
    else aux (a :: acc) (a + 1) b
  in
    if p <= q then List.rev (aux [] p q)
    else aux [] q p


(* Without List.rev, but UGLY *)
let range a b =
  if a <= b then
    let rec aux acc i =
      if i >= a then aux (i :: acc) (i - 1)
      else acc
    in
    aux [] b
  else
    let rec aux acc i =
      if i <= a then aux (i :: acc) (i + 1)
      else acc
    in
    aux [] b

(* Improved by making use of the features of FP*)
let range a b =
  let (op1, op2) = 
    if a <= b then ((>=), (-))
    else ((<=), (+))
  in
  let rec aux acc i =
    if op1 i a then aux (i :: acc) (op2 i 1)
    else acc
  in
  aux [] b
;;
