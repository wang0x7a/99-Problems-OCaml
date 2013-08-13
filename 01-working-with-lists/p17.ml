(* Problem:
   Split a list into two parts; the length of the first part is given.
   If the length of the first part is longer than the entire list, then the 
   first part is the list and the second part is empty.

   Function Type:
   val split : 'a list -> int -> ('a list) * ('a list)
 *)

let split (lst : 'a list) (n : int) : ('a list) * ('a list) =
  if n > List.length lst then (lst, [])
  else
    let rec loop (tmp : 'a list) (i : int) = function
    | [] -> ([], [])
    | h :: t ->
      if i = 1 then (tmp @ [h], t)
      else loop (tmp @ [h]) (i - 1) t
    in loop [] n lst
;;

(* Improved version
   Since List.length will iterate the whole list as well, we can merge
   the if-else conditions.
 *)
let split (lst : 'a list) (n : int) : ('a list) * ('a list) =
  let rec loop (tmp : 'a list) (i : int) = function
    | [] -> (tmp, [])
    | h :: t as l ->
      if i = 0 then (tmp, l)
      else loop (tmp @ [h]) (i - 1) t
  in loop [] n lst
;;
