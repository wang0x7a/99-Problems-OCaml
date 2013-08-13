(* Problem:
   Duplicate the elements of a list.

   Function Type:
   val duplicate : 'a list -> 'a list
 *)

let duplicate (lst : 'a list) : 'a list =
  let rec loop (acc : 'a list) (l : 'a list) : 'a list =
    match l with
    | [] -> acc
    | h :: t -> loop (h :: (h :: acc)) t
  in List.rev (loop [] lst)
;;
