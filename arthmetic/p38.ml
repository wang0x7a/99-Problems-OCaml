(* Example of Unix module *)

#load "unix.cma"

let timeit f a =
  let t0 = Unix.gettimeofday() in
  ignore(f a);
  let t1 = Unix.gettimeofday() in
  t1 -. t0
;;

(******************************)
(* Some notes of ";;" and ";" *)
(******************************)
(* Rule #1:
 * use ";;" to separate statements at the top-level of code, and never within
 * function definitions or any other kind of statement.
 *
 * Rule #2:
 * Cases that we can omit ";;"
 * 1. Before the keyword let
 * 2. Before the keyword open
 * 3. Before the keywork type
 * 4. At the very end of the file
 * 5. A few other (very rare) places where OCaml can "guess" that the next
 *    thing is the start of a new statement and not the contiunation of the 
 *    current statement.
 *
 * Rule #3:
 * Consider "let ... in" as a statement, and never put a signle ; after it
 *
 * Rule #4:
 * For all other statements within a block of code, follow then with a 
 * single ;, except for the very last one.
 *
 * Note about ";"
 * ";" is an operator with type unit -> 'b -> 'b.
 * For example 1;2 will get a warning of needing unit type
 *)
