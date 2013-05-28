(* Problem:
   Extract a given number of randomly selected elements from a list.
   The selected items shall be returned in a list. We use the Random module 
   but do not initialize it with Random.self_init for reproducibility.

   val rand_select : 'a list -> int -> 'a list
 *)
exception NotFound;;

let rand_select (lst : 'a list) (num : int) : 'a list =
  (*Extract the kth element of a list and return the selected one
    and the rest list.
  *)
  let rec extract_at (k : int) (acc : 'a list) (l : 'a list) : ('a * 'a list) =
    match l with
    | [] -> raise NotFound
    | h :: t ->
      if k = 0 then (h, acc @ t)
      else extract_at (k - 1) (h :: acc) t
  in
  (* Extract a randomly selected element *)
  let rand_extract (l : 'a list) (len : int): ('a * 'a list) =
    extract_at (Random.int len) [] l
  in
  (* The main entry of this function *)
  let rec loop (acc : 'a list) (i : int) (l : 'a list) (len : int) : 'a list =
    if i = num then acc
    else
      let selected, rest = rand_extract l len in
      loop (selected :: acc) (i + 1) rest (len - 1)
  in
  let len = List.length lst in
  loop [] 0 lst len
;;
