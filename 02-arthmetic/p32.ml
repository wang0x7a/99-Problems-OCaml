(* Determine the greatest common divisor of two positive integer numbers *)

let gcd (a : int) (b : int) : int =
  let rec loop (p : int) (q : int) : int =
    if q mod p = 0 then p
    else loop q (p mod q)
  in
  if a > b then loop a b
  else loop b a
;;

(* Revisit P32 *)
let gcd a b =
  let rec loop p q =
    let r = p mod q in
    if r = 0 then q
    else loop q r
  in
  if a > b then loop a b
  else loop b a
;;
