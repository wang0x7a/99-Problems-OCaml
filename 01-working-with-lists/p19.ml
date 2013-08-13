(* Problem:
   Rotate a list N places to the left, and when the steps are negative,
   rotate to the right.

   Function Type:
   val rotate : 'a list -> int -> 'a list
 *)

let rotate (lst : 'a list) (n : int) : 'a list =
  let len = List.length lst in
  if len = 0 then []
  else 
    let n = (len + n) mod len in
    let rec loop (acc : 'a list) (i : int) = function
    | h :: t as l-> 
      if i = 0 then l @ (List.rev acc)
      else loop (h :: acc) (i - 1) t
    (* pattern [] will never be matched *)
    | others -> others
    in loop [] n lst
;;
