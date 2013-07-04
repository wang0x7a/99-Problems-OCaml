(* Problem:
 * Calculate Euler's totient function phi(m) (improved).
 * Euler's totient function phi(m) is defined as the number of positive
 * integers r (1 <= r < m) that are coprime to m. We let phi(1) = 1.
 *
 * If the list of the prime factors of a number m is known in the form of the 
 * previous problem then the function phi(m) can be effectively calculated as
 * follows: Let [(p1, m1); (p2, m2); ...] be the list of prime factors (and
 * their multiplicities) of a given number m. The phi(m) can be calculated with
 * the following formula:
 *
 * phi(m) = (p1 - 1) * p1 ^ (m1 - 1) * (p2 - 1) * p2 ^ (m2 - 1) * ...
 *)

let factors (n : int) : (int * int) list =
  let rec aux (acc : (int * int) list) (d : int) (count : int) (i : int)
  : (int * int) list =
    if i = 1 then
      if count = 0 then acc
      else (d, count) :: acc
    else
      if i mod d = 0 then aux acc d (count + 1) (i / d)
      else aux (if count = 0 then acc else (d, count) :: acc) (d + 1) 0 i
  in
  aux [] 2 0 n

(* Calculate x^y *)
let pow (x : int) (y : int) : int =
  let rec aux (acc : int) (count : int) =
    if count = 0 then acc
    else aux (acc * x) (count - 1)
  in
  aux 1 y

let phi (n : int) : int =
  let rec aux (acc : int) = function
  | [] -> acc
  | (p, m) :: t -> aux ((p - 1) * (pow p (m - 1)) * acc) t
  in
  aux 1 (factors n)
;;
