(* Problem: 
   Drop every N'th element from a list

   Function Type:
   val drop : 'a list -> int -> 'a list
 *)

(* Since lists are immutable, we can copy the exiting elements and 
   skip the specific ones.
 *)
let drop (lst : 'a list) (step : int) : 'a list =
  let rec loop (acc : 'a list) (l : 'a list) (i : int) (n : int): 'a list =
    match l with
    | [] -> acc
    | h :: t -> 
      if i = 1 then loop acc t n n
      else loop (h :: acc) t (i - 1) n
  in List.rev (loop [] lst step step)
;;
