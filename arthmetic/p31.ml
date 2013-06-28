(* Determine whether a given integer number is prime *)

let is_prime (num : int) : bool =
  let rec loop (divisor : int) : bool =
    if divisor = 1 then true
    else if num mod divisor = 0 then false
    else loop (divisor - 1)
  in loop (num / 2)
;;

(* Instead of considering each possible divisor, we can also take a pair
 * of divisors into consideration directly, reducing the time complexity 
 * from n/2 down to sqrt(n) *)
let is_prime (num : int) : bool =
  let rec loop (d : int) : bool =
    (d * d > num) || (num mod d <> 0 && loop (d + 1))
  in loop 2
;;
