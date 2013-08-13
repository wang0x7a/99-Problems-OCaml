(* Problem:
   Pack consecutive duplicates of list elements into sublists.

   Function Type:
   val pack : 'a list -> ('a list) list

   E.g.,
   pack [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`d;`e;`e;`e;`e]
   = [[`a;`a;`a;`a]; [`b]; [`c;`c]; [`a;`a]; [`d;`d]; [`e;`e;`e;`e]];;
 *)

let pack (lst : 'a list) : ('a list) list =
  let rec loop (acc : ('a list) list) (sub_acc : 'a list) (l : 'a list) : 
  ('a list) list =
    match l with
    | a :: (b :: t as tl) ->
      let sub_acc = a :: sub_acc
      in
      (*Pattern for general recursion*)
      if t != [] then
        if a = b then loop acc sub_acc tl
        else loop (sub_acc :: acc) [] tl
      (* Pattern for the last two elements *)
      else 
        if a = b then (b :: sub_acc) :: acc
        else [b] :: (sub_acc :: acc)
    (* Patterns for [] (empty list) and _ :: [] (list with only one element) *) 
    | others -> sub_acc :: acc
  in List.rev ( loop [] [] lst)
;;

(* Simplized version *)
let pack (lst : 'a list) : ('a list) list =
  let rec loop (acc : ('a list) list) (sub_acc : 'a list) (l : 'a list) :
  ('a list) list =
    match l with
    | [] -> []
    | [x] -> (x :: sub_acc) :: acc
    | a :: (b :: _ as t) ->
      if a = b then loop acc (a :: sub_acc) t
      else loop ((a :: sub_acc) :: acc) [] t
  in List.rev (loop [] [] lst)
;;
