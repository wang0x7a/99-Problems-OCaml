(* Problem:
   Generate the combinations of K distinct objects chosen from the N 
   elements of a list.

   In how many ways can a committee of 3 be chosen from a group of 12 people?
   We all know that there are C(12, 3) = 220 possibilities, and we need to 
   generate all the possibilites in a list.

   Function Type:
   val extract : int -> 'a list -> ('a list) list
 *)

(* This solution is referred to the one implemented by VictorNicollet, 
   which shows the beauty of functional programming, and the key techniques 
   are: 1) higher-order function, and 2) partial evaluation.
 *)

let extract (k : int) (lst : 'a list) : ('a list) list =
  let rec loop (i : int) (acc : ('a list) list) 
  (emit : 'a list -> ('a list) list -> ('a list) list) (l : 'a list) =
    match l with
    | [] -> acc
    | h :: t -> 
      (* for each possibility, add the last element until the list is empty*)
      if i = 1 then loop i (emit [h] acc) emit t
      else
        (* Construct a higher-order function, to reserve the emittions *)
        let emit_absorb x = emit (h :: x) in
        loop i (loop (i - 1) acc emit_absorb t) emit t
  in
  (* We could define emit before loop, but defining it here shows two 
     fancinating aspects of functional programming:
     1) partial evaluation (in loop)
     2) since functions are variables (vice versa), we can define functions
        anywhere with partial evaluation.
   *)
  let emit x acc = x :: acc in
  loop k [] emit lst
;;

(* An ugly version that does not scale *)
open Core.Std

let select_two list =
  let select_one list =
    let rec aux acc = function
      | [] -> acc
      | h :: t -> aux (([h], t) :: acc) t
    in
    aux [] list
  in
  let rec aux acc = function
    | [] -> acc
    | h :: t ->
        let rest = select_one t in
        let current = List.map ~f:(fun x -> h :: (fst x)) rest in
        aux (current @ acc) t
  in
  aux [] list
;;

(* a scalable version by my own *)
open Core.Std

let absorb rec_list =
  let emit record =
    let selected, rest = record in
    let rec aux acc = function
      | [] -> acc
      | h :: t ->
          aux ((h :: selected, t) :: acc) t
    in
    aux [] rest
  in
  let rec aux acc = function
    | [] -> acc
    | h :: t ->
        aux ((emit h) @ acc) t
  in
  aux [] rec_list

let select_n n list =
  let rec_list = [([], list)] in
  let rec aux i list =
    let new_list = absorb list in
    if i = n then new_list
    else aux (i + 1) new_list
  in
  let raw = aux 1 rec_list in
  List.map ~f:(fun x -> fst x) raw
;;
