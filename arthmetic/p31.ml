(* Determine whether a given integer number is prime *)

let is_prime (num : int) : bool =
  let rec loop (divisor : int) : bool =
    if divisor = 1 then true
    else if num mod divisor = 0 then false
    else loop (divisor - 1)
  in loop (num / 2)
