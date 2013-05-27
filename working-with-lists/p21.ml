(* Problem:
   Insert an element at a given position into a list.
   Start counting list elements with 0.

   Function Type:
   val insert_at : int -> 'a -> 'a list
 *)

let insert_at (elem : 'a) (k : int) (lst : 'a list) : 'a list =
  let rec search (acc : 'a list) (i : int) = function
  (* In VictorNicollet's solution,
     [] -> [] fails to insert an element to an empty list.
   *)
  | [] -> acc @ [elem]
  | h :: t as l->
    if i = k then (acc @ [elem]) @ l
    else search (acc @ [h]) (i + 1) t
  in search [] 0 lst
;;
