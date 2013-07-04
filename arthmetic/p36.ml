(* Problem:
 * Determine the prime factors of a given positive integer.
 * Construct a list containing the prime factors and their multiplicity.
 *)

let factors (n : int) : (int * int) list =
  let rec aux (acc : (int * int) list) (d : int) (count : int) (i : int) =
    if i = 1 then
      if count = 0 then acc
      else (d, count) :: acc
    else
      if i mod d = 0 then aux acc d (count + 1) (i / d)
      else aux (if count = 0 then acc else (d, count) :: acc) (d + 1) 0 i
  in
  List.rev (aux [] 2 0 n)
;;
