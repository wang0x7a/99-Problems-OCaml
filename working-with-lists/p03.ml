(* Problem:
   Find the kth element of a list

   Type:
   'a list -> 'a option
 *)

let rec at (lst : 'a list) (k : int): 'a option =
  match lst with
  | [] -> None
  | h :: t ->
    if k = 0 then None
    else if k = 1 then Some h
    else at t (k - 1)
;;

(* Test cases *)
assert (at [`a; `b; `c; `d] 0 = None);;
assert (at [`a; `b; `c; `d] 5 = None);;
assert (at [`a; `b; `c; `d] 1 = Some `a);;
assert (at [`a; `b; `c; `d] 2 = Some `b);;
