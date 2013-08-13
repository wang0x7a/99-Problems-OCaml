(* Problem:
 * Calculate Euler's totient function phi(m).
 *
 * Euler's so-called totient function phi(m) is defined as the number of 
 * positive integers r (1 <= r < m) that are coprime to m. We let phi(1) = 1.
 *
 * Find out what the value of phi(m) is if m is a prime number. Euler's totient
 * function plays an important role in one of the most widely used public key 
 * cryptography methods (RSA).In this exercise you should use the most 
 * primitive method to calculate this function (smarter ways will be discussed 
 * later).
 *)

let gcd (a : int) (b : int) : int =
  let rec loop (p : int) (q : int) : int =
    if q mod p = 0 then p
    else loop (q mod p) p
  in
  if a < b then loop a b
  else loop b a

let coprime (a : int) (b : int) : bool = gcd a b = 1


let phi (n : int) : int =
  let rec aux (count : int) (i : int) : int =
    if i = n then count
    else
      if coprime n i then aux (count + 1) (i + 1)
      else aux count (i + 1)
  in
  if n = 1 then 1
  else aux 0 1
;;

let phi (n : int) : int =
  let rec aux (count : int) (i : int) : int =
    if i < n then
      aux (if coprime n i then count + 1 else count) (i + 1)
    else count
  in
  if n = 1 then 1 else aux 0 1
;;
