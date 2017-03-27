(* READ THIS COMMENT!
 *
 * In this file there are various lines marked by a comment like so:
 *
 *   
 *
 * You do not need to delete these lines immediately, but they should be gone by
 * the time you hand in your homework. They are placeholders for your
 * implementations of the functions specified in the homework. Without them,
 * this file would not load.
 *
 * If you remove such a line without implementing the function it is associated
 * with, this file will not load. Only remove such lines when you are ready to
 * implement their associated function.
 *)

(* Purpose: returns true if n is even, false otherwise.
   Assumes n is a natural number *)
fun evenP (n : int) : bool =
    case n
     of 0 => true
      | 1 => false
      | _ => evenP (n-2)

(* Purpose: returns true if n is odd, false otherwise.
   Assumes n is a natural number *)
fun oddP (n : int) : bool =
    case n
     of 0 => false
      | 1 => true
      | _ => oddP (n-2)

(* Purpose: returns m + n. Assumes m and n are natural numbers. *)
fun add (m : int, n : int) =
  case m of
    0 => n
  | _ => 1 + (add (m - 1, n))

(* Task: Implement and document this function. *)
(* DOCUMENTATION GOES HERE *)

(* Purpose: returns m * m. Assumes m and n are natural numbers.
    Examples:   mult (0,0) = 0
                mult (5,0) = 0
                mult (0,6) = 0
                mult (2,2) = 4
                mult (1,11) = 11
      *)
fun mult (m : int, n : int) : int =
      case m of 
        0 => 0
      | _ => add(n,mult(m-1,n))

  (* Tests *)

 val 0 = mult (0,0)
 val 0 = mult (5,0)
 val 0 = mult (0,6)
 val 4 = mult (2,2)
 val 11 = mult (1,11) 

(* Task: Implement and document this function. *)
(* DOCUMENTATION GOES HERE *)

(* Purpose: returns H(n), the nth harmonic number. Assumes n is a natural number
      Examples: harmonic 0 = 0  (This is H(0))
                harmonic 1 = 1.0
                harmonic 2 = 1.5 
                harmonic 3 ≈ 1.833333...

  *)

fun harmonic (n : int) : real =
      case n of 
        0 => real 0
      | _ => (1.0/(real n)) + harmonic (n-1)

      (* Test *)

val true = Real.==(harmonic 0, 0.0)
val true = Real.==(harmonic 1, 1.0)
val true = (1.4999999 < harmonic 2)
val true = (1.8333333 < harmonic 3)



(* Task: Implement this function. *)
(*  DOCUMENTATION GOES HERE *)

(* Purpose: Assuming n and d are natural numbers and d > 0, divmod returns 
(q,r) such that qd + r = n and r < d. In other words, this function returns
the quotient of n div d along with the remainder 
        Example: divmod (0,1) = (0,0)
                 divmod (1,1) = (1,0)
                 divmod (15,5) = (3,0)
                 divmod (8,3) = (2,2)

  *)

fun divmod (n : int, d : int) : int * int =
      case (n < d) of 
        true => (0,n)
        | _ => let val (n',d') = (divmod(n-d,d))
                  in (1 + n', d')
                  end

(* Tests *) 

val (0,0) = divmod (0,1) 
val (1,0) = divmod (1,1) 
val (3,0) = divmod (15,5) 
val (2,2) = divmod (8,3)  
    

(* Task: Implement this function. *)
(* DOCUMENTATION GOES HERE *)

(* Purpose: Assuming n and b are natural numbers ad b > 1, sum_digits 
evaluates the sum of the digits of n in base b representation
    Examples: sum_digits(0,10) = 0
              sum_digits(15,10) = 6
              sum_digits(125,10) = 8 
              sum_digits(30,2) = 4 
*)

fun sum_digits (n : int, b : int) : int =
      case n of
          0 => 0
        | _ => let val (q,r) = divmod (n,b) 
                  in r + sum_digits(q,b)
                  end

       (* Tests *) 

      val 0 = sum_digits(0,10)
      val 6 = sum_digits(15,10)
      val 8 = sum_digits(125,10)
      val 4 = sum_digits(30,2)
      val 2 = sum_digits(129,2)



