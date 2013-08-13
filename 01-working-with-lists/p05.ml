(* Problem:
   Reverse a list

   Function type:
   'a list -> 'a list
 *)

(* Non-tail-recursive version *)
let rec rev (lst : 'a list) : 'a list =
  match lst with
  | [] -> []
  | h :: t -> (rev t) @ [h]
;;

(* Tail-recursive version *)
let rev (lst : 'a list) : 'a list =
  let rec loop (acc : 'a list) (l : 'a list) : 'a list =
    match l with
    | [] -> acc
    | h :: t -> loop (h :: acc) t
  in loop [] lst
;;
