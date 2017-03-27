use "lib.sml";
exception Unimplemented

(* ---------------------------------------------------------------------- *)
(* map *)

fun pluralize_rec (t : string tree) : string tree =
    case t of
        Empty => Empty
      | Leaf x => Leaf (x ^ "s")
      | Node(l,r) => Node(pluralize_rec l , pluralize_rec r)

fun mult_rec (c : int, t : int tree) : int tree =
    case t of
        Empty => Empty
      | Leaf x => Leaf (c * x)
      | Node(l,r) => Node(mult_rec (c,l) , mult_rec (c,r))

(* TASK *)

(* Purpose: map on tree t produces a tree, t', with function f applied to each 
leaf element of t at each corresponding position

Examples:

 val Empty = map(fn x => x ^ "s", Empty)
 val Node (Node (Leaf 2,Leaf 4), 
               Node (Leaf 6,Leaf 8)) = map(fn x => 2 * x, Node (Node (Leaf 1,Leaf 2), 
               Node (Leaf 3,Leaf 4)))
 val Node (Node (Leaf "as",Leaf "bs"), 
               Node (Leaf "cs",Leaf "ds")) = map(fn x => x ^ "s", Node (Node (Leaf "a",Leaf "b"), 
               Node (Leaf "c",Leaf "d")))

*) 

fun map (f : 'a -> 'b, t : 'a tree) : 'b tree = 
    case t of
      Empty => Empty
      | Leaf x => Leaf (f(x))
      | Node(l,r) => Node(map (f,l), map (f,r))

 val Empty = map(fn x => x ^ "s", Empty)
 val Node (Node (Leaf 2,Leaf 4), 
               Node (Leaf 6,Leaf 8)) = map(fn x => 2 * x, Node (Node (Leaf 1,Leaf 2), 
               Node (Leaf 3,Leaf 4)))
 val Node (Node (Leaf "as",Leaf "bs"), 
               Node (Leaf "cs",Leaf "ds")) = map(fn x => x ^ "s", Node (Node (Leaf "a",Leaf "b"), 
               Node (Leaf "c",Leaf "d")))



(* Purpose: pluralize takes a tree of strings t and produces a tree of strings
t' such that each leaf element of t' is each leaf element of t at each corresponding
position with an "s" added on 

Examples: 
val Empty = pluralize(Empty)
val  val Node (Node (Leaf "as",Leaf "bs"), 
               Node (Leaf "cs",Leaf "ds")) = pluralize(Node (Node (Leaf "a",Leaf "b"), 
               Node (Leaf "c",Leaf "d")))
*)

fun pluralize (t : string tree) : string tree = 
map(fn x => x ^ "s", t)

val Empty = pluralize(Empty)
val Node (Node (Leaf "as",Leaf "bs"), 
               Node (Leaf "cs",Leaf "ds")) = pluralize(Node (Node (Leaf "a",Leaf "b"), 
               Node (Leaf "c",Leaf "d")))
val true = (tolist(pluralize_rec(Node (Node (Leaf "a",Leaf "b"), 
               Node (Leaf "c",Leaf "d")))) = tolist(pluralize(Node (Node (Leaf "a",Leaf "b"), 
               Node (Leaf "c",Leaf "d")))))

(* Purpose: mult takes a tree of ints t and an int c, and produces a tree of ints
t' such that each leaf element of t' is each leaf element of t at each corresponding
position multiplied by c

Examples: 
val Empty = mult(5,Empty)
val Node (Node (Leaf 2,Leaf 4), 
               Node (Leaf 6,Leaf 8)) = mult(2,Node (Node (Leaf 1,Leaf 2), 
               Node (Leaf 3,Leaf 4)))

*)

fun mult (c : int, t : int tree) : int tree = 
map(fn x => c * x, t)

val Empty = mult(5,Empty)
val Node (Node (Leaf 2,Leaf 4), 
               Node (Leaf 6,Leaf 8)) = mult(2,Node (Node (Leaf 1,Leaf 2), 
               Node (Leaf 3,Leaf 4)))
val true = (tolist(mult_rec(4, Node (Node (Leaf 1,Leaf 2), 
               Node (Leaf 3,Leaf 4)))) = tolist(mult(4, Node (Node (Leaf 1,Leaf 2), 
               Node (Leaf 3,Leaf 4)))))


(* ---------------------------------------------------------------------- *)
(* reduce *)

fun sum_rec (t : int tree) : int =
    case t of
        Empty => 0
      | Leaf x => x
      | Node(t1,t2) => (sum_rec t1) + (sum_rec t2)

fun join_rec (t : string tree) : string =
    case t of
        Empty => ""
      | Leaf x => x
      | Node(t1,t2) => (join_rec t1) ^ (join_rec t2)

(* TASK *)

(* Purpose: reduce takes a function of type 'a * 'a -> 'a, a value 
of type 'a to represent an empty tree, and a tree, t, and applies the 
  function to each leaf node in the tree returning a value of type 'a
representing the result of applying the function across the entire tree

Examples:


*)

fun reduce (n : 'a * 'a -> 'a, b : 'a, t : 'a tree) : 'a = 
    case t of 
      Empty => b
      | Leaf x => x
      | Node(t1,t2) => n(reduce(n,b,t1),reduce(n,b,t2))


(* TASK *)     

(* Purpose: sum takes an int tree and returns the sum of all
ints in each leaf of t such that sum(t) =~ sum_rec(t)

  Examples: val 0 = sum(Empty) 
           val 5 = sum(Node(Leaf 5, Empty))
           val 20 = sum(Node (Node (Leaf 2,Leaf 4), 
               Node (Leaf 6,Leaf 8)))
*)   

fun sum (t : int tree) : int = 
    reduce(fn(x:int,y:int) => x + y, 0, t)

    val 0 = sum(Empty) 
    val 5 = sum(Node(Leaf 5, Empty))
    val 20 = sum(Node (Node (Leaf 2,Leaf 4), 
               Node (Leaf 6,Leaf 8)))

(* Purpose: join takes a string tree and returns the concatenation of all
strings in each leaf of t from left to right 
such that join(t) =~ join_rec(t)


  Examples: val "" = join(Empty) 
            val "hello" = join(Node("hello", Empty))
            val "sam!" = join(Node (Node (Leaf "s",Leaf "a"), 
               Node (Leaf "m",Leaf "!")))
*)   


fun join (t : string tree) : string = 
    reduce(fn(a:string,b:string) => a^b, "", t)

    val "" = join(Empty) 
    val "hello" = join(Node(Leaf "hello", Empty))
    val "sam!" = join(Node (Node (Leaf "s",Leaf "a"), 
               Node (Leaf "m",Leaf "!")))


(* ---------------------------------------------------------------------- *)
(* programming with map and reduce *)

(* TASK *)

(* Purpose: flatten takes a tree,t, of 'a trees and returns a tree t' such
that t' contains all the elements in each tree of t in the same order 
  in which they occur in t

    Examples: val Empty = flatten(Empty)
              val Node(Leaf 1,Empty) = flatten(Node (Empty, Leaf (Node( Leaf 1, Empty))))
              val Node (Node (Leaf 1,Leaf 2),Node (Leaf 3,Empty)) = 
  flatten (Node (Leaf (Node (Leaf 1, Leaf 2)),
                       Node (Leaf (Leaf 3),
                             Empty)))

*)

fun flatten (t : ('a tree) tree) : 'a tree = 
  reduce(fn(t1,t2) => Node(t1,t2), Empty, t)


val Empty = flatten(Empty)
val Node(Empty, Node(Leaf 1, Empty)) = flatten(Node (Empty, Leaf (Node( Leaf 1, Empty))))         
val Node (Node (Leaf 1,Leaf 2),Node (Leaf 3,Empty)) = 
  flatten (Node (Leaf (Node (Leaf 1, Leaf 2)),
                       Node (Leaf (Leaf 3),
                             Empty)))

(* TASK *)

(* Purpose: filter takes a function p:'a -> bool and a tree, t,
and returns a tree, t', containin all elements of t for which p is true.
  These remaining elements in t' should be in the same order as they were 
    in t.

      Examples: 

          val Empty = filter(fn x => x > 2, Empty)
          val Node (Node (Empty,Empty),Node (Leaf 3,Empty)) = 
              filter (fn x => x > 2, Node (Node (Leaf 1,Leaf 2),Node (Leaf 3,Empty)))
          val [1,2,3] = filter(fn x => x < 4, fromlist([1,2,3,4,5]))


*)


fun filter (p : 'a -> bool, t : 'a tree) : 'a tree = 
  flatten(map(fn x => case p(x) of 
                    true => Leaf x
                    |false => Empty,     
                     t))


  val Empty = filter(fn x => x > 2, Empty)
  val Node (Node (Empty,Empty),Node (Leaf 3,Empty)) = 
              filter (fn x => x > 2, Node (Node (Leaf 1,Leaf 2),Node (Leaf 3,Empty)))
  val [1,2,3] = tolist(filter(fn x => x < 4, fromlist([1,2,3,4,5])))



(* TASK *)

(* Purpose: allpairs takes two trees of arbitrary types, t1 and t2, and returns 
a tree, t', of tuples (x,y) such that for every element x of t1 and every 
element y of t2, there exists an element in t' for every possible combination 
  of x and y.

    Examples:

    val [] = tolist(allpairs(Empty, Empty))
    val [] = tolist(allpairs(Empty, Node(Leaf 1, Leaf 2)))
    val [] = tolist(allpairs(Node(Leaf 1, Leaf 2), Empty)) 
    val Node (Node (Leaf (1,"a"),Leaf (1,"b")),Node (Leaf (2,"a"),Leaf (2,"b"))) =
      allpairs (Node(Leaf 1, Leaf 2), Node(Leaf "a", Leaf "b"));

    *)


fun allpairs (tree1 : 'a tree, tree2 : 'b tree) : ('a * 'b) tree =
    flatten( map(fn x => map(fn y => (x,y), tree2),tree1) )

    val [] = tolist(allpairs(Empty, Empty))
    val [] = tolist(allpairs(Empty, Node(Leaf 1, Leaf 2)))
    val [] = tolist(allpairs(Node(Leaf 1, Leaf 2), Empty)) 
    val Node (Node (Leaf (1,"a"),Leaf (1,"b")),Node (Leaf (2,"a"),Leaf (2,"b"))) =
      allpairs (Node(Leaf 1, Leaf 2), Node(Leaf "a", Leaf "b"));
    

type answers = int * int * int * int

fun same(x : int, y : int) : real = 
    case x = y of
        true => 1.0
      | false => 0.0

fun count_same ((a1,a2,a3,a4) : answers , (a1',a2',a3',a4') : answers) : real = 
    same (a1,a1') + same (a2,a2') + same (a3,a3') + same (a4,a4')

(* TASK *)

(* Purpose: my_scoring computes the percent similarity between two sets of answers. This 
scoring function will provide good results as it gives clear and speicific data on how 
  similar two sets of responses are. 

Examples: val 100.0 = my_scoring((1,1,1,1),(1,1,1,1))
  val 50.0  = my_scoring((1,1,1,1),(1,1,2,2))
  val 0.0 = my_scoring((1,1,1,1),(2,2,2,2))



*)

fun my_scoring ((a1,a2,a3,a4) : answers , (a1',a2',a3',a4') : answers) : real = 
  (((count_same((a1,a2,a3,a4),(a1',a2',a3',a4'))) / 4.0) * 100.0)

  val true = (my_scoring((1,1,1,1),(1,1,1,1)) > 99.9999)
  val true = (my_scoring((1,1,1,1),(1,1,2,2)) > 49.9999)
  val true = (my_scoring((1,1,1,1),(1,1,2,2)) < 50.0001)
  val true = (my_scoring((1,1,1,1),(2,2,2,2)) < 0.00001)


(* TASK *)

(* Purpose: matches takes a scoring function, similarity, a real, cutoff,
and a tree, people, whose elements are tuples in the form (person: string, surveyanswers: answers).
matches returns a tree of tuples (person1: string, person2: string, score:real) such that...

1. each score value is the similarity score of person1 and person2's answers
2. the returning tree is sorted from highest to lowest scores
3. the returning tree only includes pairs of ppl whose score values
    are higher than the input cutoff value
4. the returning tree contains no elements of the form (person1, person1, score)
or both (person1,person2,score) and (person2,person2,score)
      
Examples: val Empty = matches(my_scoring, 0.0, Empty)
          val Node (Leaf ("B","C",75.0),Leaf ("A","C",25.0)) = matches(my_scoring, 0.0, test_data)
          val Node (Leaf ("B", "C",75.0), Empty) = matches(my_scoring, 50.0, test_data)

*)

fun matches (similarity : answers * answers -> real,
             cutoff : real,
             people : (string * answers) tree) : (string * string * real) tree = 

let val pairtree : ((string * answers) * (string * answers)) tree = 
  allpairs(people, people)
val comptree : (string * string * real) tree = 
  map (fn ((name1,answer1),(name2,answer2)) => (name1, name2, similarity(answer1, answer2)), pairtree)
val filtertree : (string * string * real) tree =
  filter(fn ((name1,name2,score)) => name1 < name2, comptree)
val filtertree2 : (string * string * real) tree = 
  filter(fn ((name1,name2,score)) => score > cutoff, filtertree)
in sort(fn ((_,_,score1),(_,_,score2)) => Real.compare(score2,score1), filtertree2)
end

(* code for testing *)

val test_data : (string * answers) tree = fromlist [ ("A",(1,1,1,1)), ("B",(2,2,2,2)), ("C",(1,2,2,2)) ]

fun show_matches (similarity : answers * answers -> real, cutoff : real, people : (string * answers) tree) : unit =
    List.app (fn (n1,n2,score) => print (n1 ^ " and " ^ n2 
                                         ^ " have compatibility " ^ (Real.toString score ^ "\n"))) 
             (tolist (matches (similarity, cutoff, people))) 

   val Empty = matches(my_scoring, 0.0, Empty)
   val true = 
let val Node (Leaf ("B","C",x),Leaf ("A","C",y)) = matches (my_scoring, 0.0, test_data)
in Real.==(x,75.0) andalso Real.==(y,25.0)
end
(* val Node (Leaf ("B","C",3.0),Leaf ("A","C",1.0)) = matches(count_same, 0.0, test_data)  *)


