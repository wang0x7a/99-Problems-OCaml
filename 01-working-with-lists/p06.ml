(* Problem:
   Find out whether a list is a palinedrome.
   Hint: a palindrome is its own reserve

   Function Type:
   'a list -> bool
 *)

(* Naive implementation

   Space complexity: Reverse list O(n) + Compare lists O(n) = O(n)
   Time complexity: Reverse list O(n) + Compare lists O(n) = O(n)
 *)
let is_palindrome (lst : 'a list) : bool =
  lst = List.rev lst
;;

(* Query element pairs
   This implementation calls List.nth, but when the list is "too short",
   Exception "Failure nth" is thrown.

   This implementation requires constant stack size, and its time complexity 
   is O(n).
 *)
let is_palindrome (lst : 'a list) : bool =
  let rec loop (n : int) (m : int) (l : 'a list) : bool =
    if n < m then
      if List.nth l n != List.nth l m then false
      else loop (n + 1) (m - 1) l
    else true
  in loop 0 ((List.length lst) - 1) lst
;;

(* Improved version: use a user-defined nth function.

   Function Type:
   val nth (lst : 'a list) (n : int) : 'a
   
   * Time Complexisty: O(n/2)
   * Space Complexisty: O(1)
 *)

(* Notes:
   = <> : equality / inequality (deep)
   == != : equality / inequality (shallow)
 *)
let rec nth (lst : 'a list) (n : nth) : 'a =
  match lst with
  | [] -> None
  | h :: t -> 
    if n = 0 then Some h
    else nth t (n - 1)
;;

let is_palindrome (lst : 'a list) : bool =
  let rec loop (n : int) (m : int) (l : 'a list) : bool
    if n < m then
      if nth l n <> nth l m then false
      else loop (n + 1) (m - 1) l
    else true
  in loop 0 ((List.length lst) - 1) lst
;;

(* Generalize is_palindrome in a functional style:
   pass List.nth or nth as a argument

   Function Type:
   val is_palindrome (val find_nth : 'a list -> int -> 'a) (lst : 'a list) : bool
 *)

let is_palindrom (find_nth : 'a list -> int -> 'a) (lst : 'a list) : bool
  let rec loop (n : int) (m : int) (l : 'a list) : bool
    if n < m then
      if find_nth l n != find_nth l m then false
      else loop (n + 1) (m - 1) l
    else true
  in loop 0 ((List.length lst) - 1) lst
;;

