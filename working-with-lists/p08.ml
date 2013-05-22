(* Problem:
   Eliminate consecutive duplicates of list elements.

   Function Type:
   val compress : 'a list -> 'a list
 *)

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
