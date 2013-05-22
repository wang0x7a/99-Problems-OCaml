(* Problem:
   Eliminate consecutive duplicates of list elements.

   Function Type:
   val compress : 'a list -> 'a list
 *)

(* 1. Tail-recursive version *)
let compress (lst : 'a list) : 'a list =
  let rec loop (acc : 'a list) (l : 'a list) : 'a list =
    match l with
    | [] -> acc
    | h :: t ->
      match acc with
      (* works like a stack *)
      | [] -> loop (h :: acc) t
      | acc_hd :: _ ->
        if acc_hd = h then loop acc t
        else loop ( h :: acc) t
  in List.rev (loop [] lst)
;;

(* 2. Non-tail-recursive version by VictorNicollet, but more elegant. *)
let rec compress (lst : 'a list) : 'a list =
  match lst with
  | a :: (b :: _ as t) ->
    (* The difference between the Implementaion 1 and 2 is that, 1 checks the
       duplicates upon its first occurrence, while 2 checks only at the 
       boundary between two different elements.
     *) 
    if a = b then compress t
    else a :: compress t
  (* match all other cases *)
  | others -> others
;;

(* 3. Tail-recursive version of 2. *)
let compress (lst : 'a list) : 'a list =
  let rec loop (acc : 'a list) (l : 'a list) : 'a list =
    match l with
    | a :: (b :: _ as t) ->
      if a = b then loop acc t
      else loop (a :: acc) t
    (* cases for [] and [`a] *)
    | others -> others @ acc
  in List.rev (loop [] lst)
;;

