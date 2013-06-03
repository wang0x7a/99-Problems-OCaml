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
let group (persons : 'a list) (sizes : int list) : 'a list list =
  let initialize = List.map (fun size -> size, []) sizes in

  let prepend (elem : 'a) (lst : (int * 'a list) list) : 
  (int * 'a list) list list =
      
;;
