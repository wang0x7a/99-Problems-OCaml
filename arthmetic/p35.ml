(* Problem:
 * Determine the prime factors of a given positive integer.
 * Contruct a flat list containing the prime factors in ascending order
 *)

exception InvalidInput

let is_prime (n : int) : bool =
  if n <= 0 then raise InvalidInput
  else
    let rec is_not_divisor (d : int) : bool =
      d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
    in n <> 1 && is_not_divisor 2

let factors (n : int) : int list =
  if n = 1 then []
  else
    let rec aux (acc : int list) (d : int) : int list =
      if d = 1 then acc
      else if n mod d = 0 && is_prime d then aux (d :: acc) (d - 1)
      else aux acc (d - 1)
    in
    match (aux [] (n / 2)) with
    | [] -> [n]
    | l -> l
;;
