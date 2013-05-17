(* Problem:
   Implement a function that returns the length of a list, with tail recursion.

   Type:
   'a list -> int
 *)

(* Notes:
   
   Ref: http://en.wikipedia.org/wiki/Tail_call

   (1) Tail Call: a subroutine call that happens inside another procedures 
       as its final action; it may produce a return value which is then 
       immediately returned by the calling procedure.
       call site => tail position

   (2) Tail Recursion: The subroutine in tail position leads to this same 
       subroutine being called again down the call chain. Special case for 
       both recursion and tail call.

       (2.1) Significance: Implementation without adding a new stack frame 
             to the call stack, and most of the frame of the current procedure 
             is not needed any more, and it can be replaced by the frame of 
             the tail call, modified as appropriate (similar to overlay,
             exec() and fork-exec technique). Moreover, it TCO often 
             asymptotically reduces stack space requirements from O(n) to O(1).

       (2.2) Tail Call Optimization (TCO, Tail Call Elinimation): A programme 
             jumps to (goto) the called subroutine, instead of a standard call 
             sequences.

       (2.3) Implementation: Anytime a function makes a recursive call, the 
             resulting value is immediately returned. Instead of pushing the
             return address of a subroutine onto the call stack, tail calls 
             do not remember the place the subroutine is called from and they 
             perform TCO to leave the stack alone and the newly called function
             will return its result directly to the original caller.

             Note that the tail call doesn't have to appear lexically after all
             other statements in the source code; it is only important that the
             calling function return immediately after the tail call, returning
             the tail call's result if any, since the calling function will 
             never get a chance to do anything after the call if the 
             optimization is performed.

             Implementations allowing an unlimited number of tail calls to be 
             active at the same moment can also be called 
             'properly tail-recursive'.

       (2.4) Applications: Continuation Passing Style (CPS)
             Ref: http://en.wikipedia.org/wiki/Continuation_passing_style
             TBC...
             In functional programming, in order to make code tail-recursive, 
             it often requires addition of an "accumulator" argument to the 
             function.

   (3) Examples:
       Ref: http://en.wikipedia.org/wiki/Tail_call#Syntactic_form
            http://en.wikipedia.org/wiki/Tail_call#Example_programs 
 *)

let length (lst : 'a list) : int =
  (* Use a parameter aux to record the returning value of subroutine *)
  let rec loop (l : 'a list) (aux : int) : int =
    match l with
    | [] -> aux
    | h :: t -> loop t (aux + 1)
  in loop lst 0
;;

(* Test cases *)
assert (length [`a; `b; `c; `d] = 4);;
assert (legnth [] = 0);;
