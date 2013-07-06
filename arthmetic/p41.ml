(* Problem 41:
 * A list of Goldbach compositions.
 *
 * Given a range of integers by its lower and upper limit, print a list of all
 * even numbers and their Goldbach composition.
 *
 * In most cases, if an even number is written as the sum of two prime numbers,
 * one of them is very small. Very rarely, the primes are both bigger than say
 * 50. Try to find out how many such cases there are in a given range.
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
  else
    let rec loop (p : int) : int * int =
      if is_prime p && is_prime (n - p) then (p, n - p)
      else loop (p + 1)
    in loop 2


let goldbach_list (a : int) (b : int) : (int * (int * int)) list =
  let upper = if a < b then b else a in
  let rec aux (acc : (int * (int * int)) list) (i : int) =
    if i > upper then acc
    else
      if i mod 2 = 0 then aux ((i, goldbach i) :: acc) (i + 2)
      else aux acc (i + 1)
  in
  if a < b then aux [] a
  else aux [] b

exception GoldbachConjectureFails

let goldbach_list_limit (a : int) (b : int) (limit : int)
: (int * (int * int)) list =
  let upper = if a < b then b else a in
  let rec aux (acc : (int * (int * int)) list) (i : int) =
    if i > upper then acc
    else
      if i mod 2 = 0 then
        match goldbach i with
        | (p, q) -> 
          if q >= limit && p >= limit then aux ((i, (p, q)) :: acc) (i + 2)
          else aux acc (i + 2)
        | _ -> raise GoldbachConjectureFails
      else aux acc (i + 1)
  in
  if a < b then aux [] a
  else aux [] b
;;
