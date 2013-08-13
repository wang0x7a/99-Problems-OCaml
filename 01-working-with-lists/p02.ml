(* Problem:
   Find the last but one (last and penultmate) elements of a lit.

   Type:
   'a list -> ('a * 'a) option

 *)

let rec last_two (lst : 'a list) : ('a * 'a) option =
  match lst with
  | [] | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: t -> last_two t
;;

(* Test cases *)
assert ((last_two [`a; `b; `c; `d]) = Some (`c, `d));;
assert ((last_two [`a] = None));;
