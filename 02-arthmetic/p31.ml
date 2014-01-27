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
exception InvalidNumber

let is_prime (num : int) : bool =
  if num <= 0 then raise InvalidNumber
  else
    let rec loop (d : int) : bool =
      (d * d > num) || (num mod d <> 0 && loop (d + 1))
    in num <> 1 && loop 2
;;

(* Revisit *)
let is_prime n =
  let rec loop i =
    if i * i <= n then
      if n mod i <> 0 then loop (i + 1)
      else false
    else
      true
  in
  n <> 1 && loop 2
;;
