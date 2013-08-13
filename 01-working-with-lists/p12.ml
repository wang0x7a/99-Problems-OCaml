(* Problem:
   Given a run-length code list generated as specified in P11, construct 
   its uncompressed version.

   type 'a elem =
     | One of 'a
     | Many of (int * 'a)

   Function Type:
   val decode : ('a elem) list -> 'a list
 *)

type 'a elem =
  | One of 'a
  | Many of (int * 'a)
;;

let decode (lst : ('a elem) list) : 'a list =
  let rec loop (acc : 'a list) (l : ('a elem) list) : 'a list =
    match l with
    | [] -> acc
    | h :: t -> 
      match h with
      | One x -> loop (x :: acc)  t
      | Many (c, x) -> 
        let rec append (sub_acc : 'a list) (x : 'a) (count : int) : 'a list =
          if count = 0 then sub_acc
          else append (x :: sub_acc) x (count - 1)
        in loop (append acc x c) t
  in List.rev (loop [] lst)
;;
