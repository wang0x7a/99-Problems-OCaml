(* Problem:
   Find the number of elements of a list

   Type:
   'a list -> int
 *)

let rec length (lst : 'a list) : int =
  match lst with
  | [] -> 0
  | _ :: t -> 1 + length t
;;

(* Test cases *)
assert (length [] = 0);;
assert (length [`a; `b; `c] = 3);;
