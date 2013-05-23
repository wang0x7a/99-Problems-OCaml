(* Problem:
   Run-length encoding of a list

   Function Type:
   val encode : 'a list -> (int * 'a) list
 *)

let encode (lst : 'a list) : (int * 'a) list =
  let rec loop (acc : (int * 'a) list) (count : int) (l : 'a list) : (int * 'a) list =
    match l with
    | a :: (b :: _ as t) ->
      if a = b then loop acc (count + 1) t
      else loop ((count + 1, a) :: acc) 0 t
    | [] -> acc
    | [x] -> (count + 1, x) :: acc
  in List.rev (loop [] 0 lst) 
;;
