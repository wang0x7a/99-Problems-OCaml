(* Problem:
   Modify the result of the p09 in such a way that if an element has no 
   duplicates it is simply copied into the result list. Only elements with 
   duplicates are tranferred as (N E) lists.

   Since OCaml lists are homogeneous, one needs to define a type to hold
   both single elements and sub-lists.

   type 'a elem = 
     | One of 'a
     | Many of (int * 'a)

   Function Type:
   val encode : 'a list -> 'a elem list
 *)

type 'a elem = 
  | One of 'a
  | Many of (int * 'a)
;;

let encode (lst : 'a list) : ('a elem) list =
  let rec loop (acc : ('a elem) list) (count : int) (l : 'a list)
  : ('a elem) list =
    match l with
    | [] -> []
    | [x] ->
      if count = 0 then (One x) :: acc  
      else Many (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
      if a = b then loop acc (count + 1) t
      else
        if count = 0 then loop (One a :: acc) 0 t
        else loop (Many (count + 1, a) :: acc) 0 t
  in List.rev (loop [] 0 lst)
;;
