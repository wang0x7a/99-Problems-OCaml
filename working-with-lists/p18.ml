(* Problem:
   Extract a slice from a list.
   Given two indices, i and k, the slice is the list containing the elements 
   between the ith and the kth element of the original list (both limits 
   included). Start counting the elements with 0 (thisis the way the List 
   module numbers elements).

   Function Type:
   val slice : 'a list -> int -> int -> 'a list
 *)

let slice (lst : 'a list) (i : int) (k : int) : 'a list =
  let rec loop (acc : 'a list) (count : int) = function
  | [] -> acc
  | h :: t -> 
    if count >= i then
      if count <= k then loop (acc @ [h]) (count + 1) t
      else acc
    else loop acc (count + 1) t
  in loop [] 0 lst
;;
