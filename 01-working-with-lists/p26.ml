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
