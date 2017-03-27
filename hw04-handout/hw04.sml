(* remove this when you're done to make sure you didn't miss anything *)
exception Unimplemented;

use "lib.sml";

(* ---------------------------------------------------------------------- *)
(* quicksort on lists *)

(* Task *)
(* Purpose: filter_l takes list l as an argument 
and returns a list with all the elements of l that are either GEQ or LT to the pivit p. The 
argument r determines the appropriate relation

Examples: val [] = filter_l([],4,LT)
          val [1,2,3] = filter_l([1,2,3],4,LT)
          val [] = filter_l([1,2,3],4,GEQ)
          val [2,3] = filter_l([1,2,3],2,GEQ)
*)

fun filter_l (l : int list, p:int, r:rel) : int list = 
  case l of 
    [] => []
    |x::xs => case r of 
                  LT => (case x<p of
                          true => x::filter_l(xs, p, r)
                          | false => filter_l(xs, p, r))
                  | GEQ => (case x<p of
                          false => x::filter_l(xs, p, r)
                          | true => filter_l(xs, p, r))

            (* Tests *)

      val [] = filter_l([],4,LT)
      val [1,2,3] = filter_l([1,2,3],4,LT)
      val [] = filter_l([1,2,3],4,GEQ)
      val [2,3] = filter_l([1,2,3],2,GEQ)

(* Purpose: quicksort_l takes list l as an argument and returns a permutation of 
l that is sorted in increasing order and contains the exact same elements
of the original list l

  Examples: val [] = quicksort_l([])
            val [3] = quicksort_l([3])
            val [1,2,3] = quicksort_l([1,2,3])
            val [1,2,3] = quicksort_l([3,2,1])
            val [3,3,3,5] = quicksort_l([3,5,3,3])
*) 

fun quicksort_l (l : int list) : int list = 
  case l of
    [] => []
    | x::[] => [x] 
    | x::xs => quicksort_l(filter_l(xs,x,LT))@(x::quicksort_l(filter_l(xs,x,GEQ)))


    (*Tests*)

            val [] = quicksort_l([])
            val [3] = quicksort_l([3])
            val [1,2,3] = quicksort_l([1,2,3])
            val [1,2,3] = quicksort_l([3,2,1])
            val [3,3,3,5] = quicksort_l([3,5,3,3])

(* ---------------------------------------------------------------------- *)
(* quicksort on trees *)

(* Task *)

(* Purpose: combines two argument trees into one resulting tree such that:
  1. combine(T1,T2) is valuable and 
  contains all elements of T1 and T2 with no other added elements

  2. depth(combine(T1,T2)) ≤ 1 + max(depth T1, depth T2)

  3. combine (T1,T2) has a running time of O(d1) where d1 is the depth of T1

  Examples:

      val Empty = combine (Empty,Empty)
      val Node(Empty, 5, Empty) = combine (Empty, Node(Empty, 5, Empty) )
      val Node(Empty, 5, Node(Empty, 6, Empty))  = combine(Node(Empty, 5, Empty), Node(Empty, 6, Empty))
      val [1,3,2,4,5,6] = tolist(combine(fromlist([1,2,3]),fromlist([4,5,6])))

*)

fun combine (t1 : tree, t2 : tree) : tree = 
  case t1 of
    Empty => t2
    | Node(l,x,r) => Node(combine(l,r),x,t2)

    (*Tests*)

      val Empty = combine (Empty,Empty)
      val Node(Empty, 5, Empty) = combine (Empty, Node(Empty, 5, Empty) )
      val Node(Empty, 5, Node(Empty, 6, Empty))  = combine(Node(Empty, 5, Empty), Node(Empty, 6, Empty))
      val [1,3,2,4,5,6] = tolist(combine(fromlist([1,2,3]),fromlist([4,5,6])))


(* Task *)

(* Purpose: filter takes a tree t, a pivot p of type int int, and a rel r as arguments and returns a tree
such that

1. For r = LT, filter(t,p,r) is a tree with all and only elements of t less than p
  For r = GEQ, filter(t,p,r) is a tree with all and only elements of t greater than or equal to p

2. depth(filter(t,p,r)) ≤ depth(t)

3. filter(t,p,r) must have an O(d^2) span, where d is the depth of t. If t is balanced,
work must be O(n) where n is the size of t

Examples:  val Empty = filter(Empty, 5, LT)
  val Empty = filter(fromlist([1,2,3]), 5, GEQ)
  val Node (Node (Empty,1,Empty),2,Node (Empty,3,Empty)) = filter(fromlist([1,4,2,3,7]),4, LT)
  val Node (Empty,4,Node (Empty,7,Empty)) = filter(fromlist([1,4,2,3,7]),4, GEQ)

*) 




fun filter (t : tree, i : int, r : rel) : tree = 
  case t of
    Empty => Empty
    | Node(left,x,right) => case r of
                        LT => (case x<i of
                                    true => Node(filter(left,i,r),x,filter(right,i,r))
                                    | false => combine(filter(left,i,r),filter(right,i,r))
                                    )

                        | GEQ => (case x<i of
                                    false => Node(filter(left,i,r),x,filter(right,i,r))
                                    | true => combine(filter(left,i,r),filter(right,i,r))
                                     )

  (*Tests*)

  val Empty = filter(Empty, 5, LT)
  val Empty = filter(fromlist([1,2,3]), 5, GEQ)
  val Node (Node (Empty,1,Empty),2,Node (Empty,3,Empty)) = filter(fromlist([1,4,2,3,7]),4, LT)
  val Node (Empty,4,Node (Empty,7,Empty)) = filter(fromlist([1,4,2,3,7]),4, GEQ)

(* Task *)

(* Purpose: quicksort_t takes a tree, t, and returns a tree that is sorted and contains all and only elements 
from t. 
  Examples: val true = issorted(quicksort_t(fromlist([1,5,2,5,7,9])))
            val Empty = quicksort_t(Empty)
            val [1] = tolist(quicksort_t(Node(Empty,1,Empty)))
            val [1,2,5,5,7,9] = tolist(quicksort_t(fromlist([1,5,2,5,7,9])))


*)

 fun quicksort_t (t : tree) : tree = 
  case t of
    Empty => Empty 
    | Node(left,x,right) => let
      val lessleft = filter(left,x,LT)
      val lessright = filter(right,x,LT)
      val moreleft = filter(left,x,GEQ)
      val moreright = filter(right,x,GEQ)
    in
      Node(quicksort_t(combine(lessleft,lessright)), x, quicksort_t(combine(moreleft,moreright)) )
    end

  (*Tests*)

    val true = issorted(quicksort_t(fromlist([1,5,2,5,7,9]))) 
    val Empty = quicksort_t(Empty)
    val [1] = tolist(quicksort_t(Node(Empty,1,Empty)))
    val [1,2,5,5,7,9] = tolist(quicksort_t(fromlist([1,5,2,5,7,9])))



(* ---------------------------------------------------------------------- *)
(* rebalance *)

(* Task *)

(* Purpose: takeanddrop takes an tree, t, and an int, i,
 and returns two tree's, t1 and t2, such that t1 contains all
 and only the leftmost i elements of t in the original order and 
 t2 contains all the remaining elements of t in original order.

Examples: 

val (Empty,Empty) = takeanddrop(Empty, 4)
val (Node(Node(Empty,1,Empty),2,Empty),Node(Empty,3,Empty)) = takeanddrop (Node (Node (Empty,1,Empty),2,Node (Empty,3,Empty)),2)
val (Node (Node (Empty,1,Empty),2,Node (Empty,3,Empty)), Node (Empty,4,Node (Node (Empty,5,Empty),6,Empty))) =
  takeanddrop (Node(Node (Node (Empty,1,Empty),2,Node (Empty,3,Empty)),4,Node (Node (Empty,5,Empty),6, Empty)),3)


 *)

fun takeanddrop (t : tree, i : int) : tree * tree = 
  case t of
    Empty => (Empty, Empty)
    | Node(l,x,r) => case size(l) < i of 
                              true => let val rest : int = i - size(l) - 1 
                                          val (rl, rr) = takeanddrop(r, rest)
                              in (Node(l,x,rl), rr)
                              end
                              | false => let val (ll,lr) = takeanddrop(l,i) 
                                                            in (ll, Node(lr,x,r))
                                                            end
                    
   
val (Empty,Empty) = takeanddrop(Empty, 4)
val (Node(Node(Empty,1,Empty),2,Empty),Node(Empty,3,Empty)) = takeanddrop (Node (Node (Empty,1,Empty),2,Node (Empty,3,Empty)),2) 
val (Node (Node (Empty,1,Empty),2,Node (Empty,3,Empty)), Node (Empty,4,Node (Node (Empty,5,Empty),6,Empty))) =
  takeanddrop (Node(Node (Node (Empty,1,Empty),2,Node (Empty,3,Empty)),4,Node (Node (Empty,5,Empty),6, Empty)),3) 



(* the rest of rebalance in terms of your takeanddrop *)
fun halves (t : tree) : tree * int * tree =
    let
      val (l , vr) = takeanddrop (t , (size t) div 2)
      val (Node (Empty, v , Empty) , r) = takeanddrop (vr , 1)
    in
      (l , v , r)
    end

fun rebalance (t : tree) : tree =
    case t
     of Empty => Empty
      | _ =>
        let
          val (l , x , r) = halves t
        in
          Node (rebalance l , x , rebalance r)
        end

        
