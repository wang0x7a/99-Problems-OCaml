(* Flatten a nested list structure
   There is no nested list type in OCaml, so we need to define one first.
   A node of a nested list is either an element, or a list of nodes.

   E.g.,
   type 'a node =
     | One of 'a
     | Many of 'a node list
   ;;

   Function Type:
   val flatten (lst : 'a node list) -> 'a list

         Nested list         => Flattened list
            root
           /    \
        One      Many        => ['a; 'b; 'c; 'd; 'e]
         |     /   \   \
        'a   One  Many  One
              |   / \    |
             'b  One One 'e
                  |   |
                 'c  'd
 *)

type 'a node = 
  | One of 'a
  | Many of 'a node list
;;

let flatten (lst : 'a node list) : 'a list =
  let rec aux (acc : 'a list) (l : 'a node list) : 'a list =
    match l with
    | [] -> acc
    (* Since only one element will be appended to the end of acc,
       its complexity should be the same as :: operation.
     *)
    | (One h) :: t -> aux (acc @ [h]) t
    | (Many h) :: t -> aux (aux acc h) t
  in aux [] lst
;;
