(* Problem 39:
 * A list of prime numbers.
 * Given a range of integers by its lower and upper limit, construct a list 
 * of all prime numbers in that range.
 *)

(* Determine the prime factors of an integer *)
let factors (n : int) : (int * int) list =
  let rec aux (acc : (int * int) list) (d : int) (count : int) (i : int)
  : (int * int) list =
    if i = 1 then
      if count = 0 then acc
      else (d, count) :: acc
    else
      if i mod d = 0 then aux (acc) d (count + 1) (i / d)
      else aux (if count = 0 then acc else (d, count) :: acc) (d + 1) 0 i
  in
  aux [] 2 0 n

(* Euler's phi function *)
(* 1. Caluclate x^y *)
let pow (x : int) (y : int) : int =
  let rec aux (acc : int) (i : int) : int =
    if i = 0 then acc
    else aux (x * acc) (i - 1)
  in
  aux 1 y

(* 2. Calculate phi(n) *)
let phi (n : int) : int =
  if n = 1 then 1
  else
    let rec aux (acc : int) = function
    | [] -> acc
    | (d, count) :: t ->
      aux ((d - 1) * (pow d (count - 1)) * acc) t
    in
    aux 1 (factors n)

let all_primes (a : int) (b : int) : int list =
  let upper = if a < b then b else a in
  let rec loop (acc : int list) (i : int) : int list =
    if i = upper + 1 then acc
    else loop (if phi i = i - 1 then i :: acc else acc) (i + 1)
  in
  if a < b then loop [] a
  else loop [] b
;;
