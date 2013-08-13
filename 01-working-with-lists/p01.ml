(* Problem:
   Write a function last : 'a list -> 'a option 
   that returns the last element of a list 
 *)

(* Notes
   (1) 'a option
      Ref: http://caml.inria.fr/pub/docs/u3-ocaml/ocaml-core.html
      Data types may also be parametric, this is, some of their constructors 
      may take arguments of arbitrary types. In this case, the type of these
      arguments must be shown as an argument to the type (symbol) of the data 
      structure. For instance, OCaml pre-defines the 'option' tye as follows:
      
      type 'a option = Some of 'a | None
  
      The option type can be used to get inject values v of type 'a into Some(v)
      of type 'a option iwth an extra value None.
 
 *)

let rec last (lst : 'a list) : 'a option =
  match lst with
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t
;;

assert (last [`a; `b; `c; `d] = Some `d);;
assert (last [] = None);;
