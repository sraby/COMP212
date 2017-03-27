
(* ---------------------------------------------------------------------- *)
(* SECTION 2 *)


(* Purpose: pairs the nth element of an int list with the nth
element of a string list in a new list of pairs. If one argument 
list is longer than the other, the return list should be as long 
as the shorter argument list
        Examples: val [] = zip([],[]) 
                  val [] = zip([1],[]) 
                  val [] = zip([],["g"]) 
                  val [(5,"g")] = zip([5],["g"])
                  val [(1,"a"),(2,"b"),(3,"c")] = zip([1,2,3,4],["a","b","c"])
    *)
fun zip (l1 : int list, l2 : string list) : (int * string) list =
        case (l1,l2) of 
                ([],_) => []
                | (_,[]) => []
                | (x::xs, y::ys) => (x,y)::zip(xs,ys)

    (* Tests *)

    val [] = zip([],[]) 
    val [] = zip([1],[]) 
    val [] = zip([],["g"]) 
    val [(5,"g")] = zip([5],["g"])
    val [(1,"a"),(2,"b"),(3,"c")] = zip([1,2,3,4],["a","b","c"])
    

(* Purpose: takes a list of tuples and returns a tuple of lists, the first containing the 
1st elements of each tuple, and the second containing the 2nd elements of each tuple 
        Examples: val ([],[]) = unzip([])
                  val ([1],["g"]) = unzip([(1,"g")])
                  val ([1,2,3],["a","b","c"]) = unzip([(1,"a"),(2,"b"),(3,"c")])

*)

fun unzip (l : (int * string) list) : int list * string list =
    case l of
        [] => ([],[])
        | x::xs => let val (a, b) = x   
                    val (u1, u2) = unzip(xs) in
                                        (a::u1,b::u2)
                                    end
                                 

        (* Tests *) 

val ([],[]) = unzip([])
val ([1],["g"]) = unzip([(1,"g")])
val ([1,2,3],["a","b","c"]) = unzip([(1,"a"),(2,"b"),(3,"c")])


(* ---------------------------------------------------------------------- *)
(* SECTION 3 *) 


(* Purpose: lasHelp takes 3 arguments: l, the tail of an int list, x, the int 
of the current run, and acc, the number of instances x has been seen in the current 
run. lasHelp scans through the int list and stops once the run is finished. lasHelp
then returns the remaining int list l, and the total length of the completed run
    Examples: val ([], 0) = lasHelp([], 0, 0)
              val ([2,1,5],2) = lasHelp([2,1,5], 9, 2)
              val ([1,1,1],4) = lasHelp([3,3,3,3,1,1,1], 3, 0)

*)
fun lasHelp (l : int list, x : int, acc : int) : int list * int =
    case l of
      [] => ([], acc)
      | p::ps => case p = x of
                            false => (p::ps, acc)
                            | true => lasHelp(ps, x, acc + 1)

        (* Tests *)

        val ([], 0) = lasHelp([], 0, 0)
        val ([2,1,5],2) = lasHelp([2,1,5], 9, 2)
        val ([1,1,1],4) = lasHelp([3,3,3,3,1,1,1], 3, 0)


(* Purpose: look_and_say takes an int list and produces a "look-and-say" sequence
of the argument int list. That is to say that the resulting int list can be read as a 
sequence of pairs such that the first number in each pair describes how many of the 
second number appear in order inthe argument list
    Examples: val [] = look_and_say([]) 
              val [1,2,1,5] = look_and_say([2,5])
              val [4,2,2,6] = look_and_say([2,2,2,2,6,6])
*)


fun look_and_say (l : int list) : int list = 
    case l of 
      [] => [] 
      | q::qs => let val (remains, count) = lasHelp(q::qs, q, 0) in
                                  count::q::look_and_say(remains)
                                end

        (* Tests *)

        val [] = look_and_say([]) 
        val [1,2,1,5] = look_and_say([2,5])
        val [4,2,2,6] = look_and_say([2,2,2,2,6,6])

   

(* ---------------------------------------------------------------------- *)
(* SECTION 4 *)

(* Purpose: add n to each element of the list l
 * (the raiseBy function from lecture with a different name).
 * Examples:
 *
 * add_to_each ([], 7) == nil
 * add_to_each (1::2::3::[], 3) == 4::5::6::[]
 * add_to_each (6::5::4::[], ~3) == 3::2::1::[] *) 
 
fun add_to_each (l : int list, n : int) : int list =
    case l of
        [] => []
      | x::xs => x + n :: add_to_each (xs, n)

val [] = add_to_each ([], 7)
val 4::5::6::nil = add_to_each (1::2::3::[], 3)
val 3::2::1::nil = add_to_each (6::5::4::[], ~3)


(* Purpose: computes the list of prefix sums for the argument list.  The
 *          i-th int in the result list is the sum of the first i int's
 *          in the argument list.
 * Examples:
 *  prefixSum [] ==> []
 *  prefixSum (1::2::3::[]) ==> 1::3::6::[]
 *  prefixSum (5::3::1::[]) ==> 5::8::9::[] 
 *)

fun prefixSum (l : int list) : int list =
    case l of
      [] => []
    | x::xs => x :: add_to_each (prefixSum xs, x)

(* Tests for prefixSum *)

val [] = prefixSum []
val [1,3,6] = prefixSum [1,2,3]
val [5,8,9] = prefixSum [5,3,1]



(* Purpose: prefixSumHelp takes an int list, l, and an int, acc, and returns a new 
list where the i-th element of the resulting list equals the sum of the first i terms 
of l, plus acc. Thus, prefixSumHelp(l,acc) =~ add_to_each(prefixSum(l),acc).
    Examples: val [] = prefixSumHelp ([], 5)
            val [1,3,6] = prefixSumHelp ([1,2,3], 0)
            val [3,5,8] = prefixSumHelp ([1,2,3], 2)

    *)

fun prefixSumHelp (l : int list, acc: int) : int list = 
  case l of 
    [] => []
    | x::xs => (x+acc)::prefixSumHelp(xs, x+acc)

  (* Tests *)

val [] = prefixSumHelp ([], 5)
val [1,3,6] = prefixSumHelp ([1,2,3], 0)
val [3,5,8] = prefixSumHelp ([1,2,3], 2)

(* Purpose: prefixSumFast computes the list of prefix sums for list l.  The j-th element 
in the resulting list is the sum of the first j terms in l.
  Examples: val [] = prefixSumFast []
val [1,3,6] = prefixSumFast [1,2,3]
val [5,8,9] = prefixSumFast [5,3,1] *)


fun prefixSumFast (l : int list) : int list =
    prefixSumHelp(l, 0)

    (* Tests *)

val [] = prefixSumFast []
val [1,3,6] = prefixSumFast [1,2,3]
val [5,8,9] = prefixSumFast [5,3,1]


(* ---------------------------------------------------------------------- *)
(* SECTION 5 *)

(* Purpose: subset_sum returns true iff the sum of a subset of ints from l, the
argument int list, adds up to s, the argument int. subset_sum returns false if 
there does not exist a subset of l that sums to s. 
        Examples: val true = subset_sum([],0)
                  val false = subset_sum([],2)
                  val true = subset_sum([1,4,~1,7], 10)
                  val false = subset_sum([1,1,0,4], 3)

*)

fun subset_sum (l : int list, s : int) : bool =
    case l of
          [] => (case (s = 0) of 
                      true => true
                      | false => false)
          | x::xs => case (subset_sum(xs, s-x)) of
                            true => true 
                            | false => subset_sum(xs, s)


      (* Tests *)

      val true = subset_sum([],0)
      val false = subset_sum([],2)
      val true = subset_sum([1,4,~1,7], 10)
      val false = subset_sum([1,1,0,4], 3)






