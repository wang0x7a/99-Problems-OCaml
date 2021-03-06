(* Problem:
   Group the elements of a set into disjoint subsets.

   a) In how many ways can a group of 9 people work in 3 disjoint subgroups
      of 2, 3 and 4 persions? Write a funciton that generates all the 
      possibilies.

   b) Generalize the above function in a way that we can specify a list of 
      group sizes and the function will return a list of groups.

   Function Type:
   val group : 'a list -> int list -> ('a list) list
 *)

(* Referring to the solution by VitorNicollet.
   The key proedure of this algorithm is to insert each element of persons 
   to every possible position of a combination template, which is initialized 
   by the input of sizes (in list). For example, if sizes = [2;1], the template
   is [2, []; 1, []], and we need reserve this template after each recursion 
   for the next one.

   Some fancinating features in this solution:
   (1) Anonymous function
   (2) Important List functions, e.g., List.map, List.filter, List.for_all
   (3) Higher-order function (again). I find that it is very useful for the 
       recursion when we need to reserve the previous elements.
*)
let group (persons : 'a list) (sizes : int list) : 'a list list list =
  let initialize = List.map (fun size -> size, []) sizes in

  (* Prepend accepts a single element (elem) and insert to each possible groups 
     (lst) that can support it (n > 0), and reserves every possibility. E.g.,
     x -> [1,[a]; 2, [b]] -> [[0, [x, a]; 2, [b]]; [1, [a]; 1, [x, b]]]
  *)
  let prepend (elem : 'a) (lst : (int * 'a list) list) : 
  (int * 'a list) list list =

    let reserve (l : (int * 'a list) list) (acc : (int * 'a list) list list) :
    (int * 'a list) list list = l :: acc
    in

    (* main entry of prepend *)
    let rec loop (acc : (int * 'a list) list list) reserve = function
      (* tail recursion, reserving the template *)
      | [] -> reserve [] acc
      (* bind important variables *)
      | ((n, l) as h) :: t ->
        let acc = if n > 0 then reserve ((n - 1, elem :: l) :: t) acc else acc in
        loop acc (fun l acc -> reserve (h :: l) acc) t
    in loop [] reserve lst
  in

  (* main loop of group *)
  let rec loop (l : 'a list) : (int * 'a list) list list =
    match l with
    (* creating the first template with sizes list *)
    | [] -> [ initialize ]
    | h :: t -> List.concat (List.map (prepend h) (loop t))
  in
  
  let all = loop persons in
  let complete = List.filter (List.for_all (fun (x, _) -> x = 0)) all in
  List.map (List.map snd) complete
;;

(* Revisit this problem *)
open Core.Std

(* Allocate an item to the groups in a template, and return all the combinations *)
let allocate template item =
  let rec aux acc emit = function
    | [] -> acc
    | ((n, l) as h) :: t ->
        let new_emit x = emit (x @ [h]) in
        aux ((emit [(n - 1, item :: l)] t) :: acc) new_emit t
  in
  let emit x acc = acc @ x in
  aux [] emit template
;;

(* A version by my own *)
let group items sizes =
  let init = List.map ~f:(fun size -> size, []) sizes in
  let allocate_one template item =
    let rec aux acc emit = function
      | [] -> acc
      | ((n,l) as h) :: t ->
          let new_emit x = emit (x @ [h]) in
          let acc = 
            if n > 0 then (emit [(n - 1, item :: l)] t) :: acc
            else acc
          in
          aux acc new_emit t
    in
    let emit x l = l @ x in
    aux [] emit template
  in
  let rec allocate acc = function
    | [] -> acc
    | h :: t -> 
        let acc = List.concat_map ~f:(fun x -> allocate_one x h) acc in
        allocate acc t
  in
  allocate [init] items
;;
