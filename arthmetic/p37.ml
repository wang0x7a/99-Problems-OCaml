(* Problem:
 * Calculate Euler's totient function phi(m) (improved).
 * Euler's totient function phi(m) is defined as the number of positive
 * integers r (1 <= r < m) that are coprime to m. We let phi(1) = 1.
 *
 * If the list of the prime factors of a number m is known in the form of the 
 * previous problem then the function phi(m) can be effectively calculated as
 * follows: Let [(p1, m1); (p2, m2); ...] be the list of prime factors (and
 * their multiplicities) of a given number m. The phi(m) can be calculated with
 * the following formula:
 *
 * phi(m) = (p1 - 1) * p1 ^ (m1 - 1) * (p2 - 1) * p2 ^ (m2 - 1) * ...
 *)

