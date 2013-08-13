(* Problem:
   Replicate the elements of a list a given number of times.

   Function Type:
   val replicate : 'a list -> int -> 'a list
 *)

let replicate (lst : 'a list) (times : int) : 'a list =
  let rec many (acc : 'a list) (n : int) (x : 'a) : 'a list =
    if n = 0 then acc
    else many (x :: acc) (n - 1) x
  in
  let rec loop (acc : 'a list) (l : 'a list) : 'a list =
    match l with
    | [] -> acc
    | h :: t -> loop (many acc times h) t
  in List.rev (loop [] lst)
;;
