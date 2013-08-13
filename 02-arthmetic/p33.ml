(* Problem:
 * Determine whether two positive integer numbers are coprime.
 * Two numbers are coprime if their greatest common divisor equals 1.
 *)

let gcd (a : int) (b : int) : int =
  let rec loop (p : int) (q : int) : int =
    if q mod p = 0 then p
    else loop (q mod p) p
  in
  if a > b then loop b a
  else loop a b

let coprime (a : int) (b : int) : bool =
  gcd a b = 1
;;
