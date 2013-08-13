(* Problem 40:
 * Goldbach's conjecture.
 *
 * Goldbach's conjecture says that every positive even number greater than 2 
 * is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most
 * famours facts in number theory that has not been proved to be correct in the
 * general case. However, it has been numerically confirmed up to very large 
 * numbers. Write a function to find the two prime numbers that sum up to a 
 * given even integer.
 *)

let is_prime (n : int) : bool =
  if n = 1 then false
  else
    let rec is_not_divisor (d : int) : bool =
      d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
    in is_not_divisor 2

exception InvalidInput

let goldbach (n : int) : int * int =
  if n mod 2 = 1 then raise InvalidInput
  else if n = 4 then (2, 2)
  else
    let rec loop (i : int) : int * int =
      let p = 2 * i + 1 in
      if (is_prime p) && (is_prime (n - p)) then (p, n - p)
      else loop (i + 1)
    in loop 1
;;

(* Solution by VictorNicollet 
 * Mine is slightly faster (but not too much) than Victor's,
 * since is_prime check for even numbers won't take too much time.
 *)
let goldbach (n : int) : int * int =
  let rec aux (d : int) : int * int =
    if (is_prime d) && (is_prime (n - d)) then (d, n - d)
    else aux (d + 1)
  in aux 2
;;
