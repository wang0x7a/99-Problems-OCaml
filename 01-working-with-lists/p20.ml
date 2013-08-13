(* Problem:
   Remove the K'the element from a list.
   The first element of the list is numbered 0, the second 1, ...

   Function Type:
   val remove_at : int -> 'a list -> 'a list
 *)

let remove_at (k : int) (lst : 'a list) : 'a list =
  let rec search (acc : 'a list) (i : int) = function
    | [] -> []
    | h :: t ->
      if i = k then acc @ t
      else search (acc @ [h]) (i + 1) t
  in search [] 0 lst   
;;
