(* Problem 44:
 * Gray Code
 *
 * An n-bit Gray code (G(n)) is a sequence of n-bit strings contstructed according
 * to the following algorithm:
 * 1) Given (n-1)-bit Gray code, G(n-1)
 * 2) Refelct G(n-1), i.e., list the entries in reverse order, then we get G'(n-1)
 * 3) Concatenate G(n) with G(n-1)
 * 4) Prefix the original G(n) with 0, and prefix G'(n-1) with 1
 *)

let prefix (c : string) (str : string) : string = c ^ str

let rec gray_code (n : int) : string list =
  if n = 1 then ["0"; "1"]
  else
    let g = gray_code (n - 1) in
    (List.map (prefix "0") g) @ (List.map (prefix "1") (List.rev g))
;;
