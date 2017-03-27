use "lib.sml";

type point = int * int (* in Cartesian x-y coordinate *)

datatype shape = 
    Rect of point * point (* bottom-left and upper-right *)
  | Disc of point * int (* center and radius *)
  | Union of shape * shape
  | Without of shape * shape
  | Translate of shape * (int * int)
  | ScaleDown of shape * (int * int) (* x factor, y factor *)
  | ScaleUp of shape * (int * int) (* x factor, y factor *)

fun squared (x : int) : int = x * x

fun containsRect ((xmin,ymin) : point, (xmax,ymax) : point) ((x,y) : point) = 
    xmin <= x andalso x < xmax andalso
    ymin <= y andalso y < ymax

(* TASK *)

(* Purpose: contains(s,p) == true if p is in the shape, or false otherwise 
  Examples: val true = contains bowtie (100,100) 
            val false = contains botwie (0,200)
            val false = contains egg (25,75)

  *)
fun contains (s : shape) ((x,y) : point) : bool = 
    case s of 
        Rect (ll,ur) => containsRect (ll, ur) (x,y)
      | Union(s1,s2) => contains s1 (x,y) orelse contains s2 (x,y)
      | Disc((cx,cy),r) => (squared(x - cx) + squared(y - cy)) < squared(r)
      | Without(s1,s2) => contains s1 (x,y) andalso not (contains s2 (x,y))
      | Translate(st, (xshift,yshift)) => contains st (x-xshift,y-yshift)
      | ScaleDown(sd, (xscaled,yscaled)) => contains sd (x*xscaled,y*yscaled)
      | ScaleUp(su, (xscaleu,yscaleu)) => contains su (x div xscaleu, y div yscaleu)




(* TASK *)

fun min(a:int,b:int):int = 
    case b < a of
      true => b
      | false => a


fun max(a:int,b:int):int = 
    case a < b of
      true => b
      | false => a

(* Purpose: boundingbox s returns (p1,p2), where points p1 and p2 define the bottom left and top right
corners of a rectangle that contain all points in s 

Example: 
val ((0,0),(200,200)) = boundingbox(bowtie)
val ((0,0),(50,150)) = boundingbox(egg) 
*)


fun boundingbox (s : shape) : point * point = 
  case s of
    Rect(ll,ur) => (ll,ur)
    | Disc((cx,cy),r) => ((cx-r,cy-r),(cx+r,cy+r))
    | Without(s1,s2) => boundingbox(s1)
    | Translate(st, (xshift,yshift)) => 
      let
        val ((x1,y1),(x2,y2)) = boundingbox(st)
      in
        ((x1+xshift,y1+xshift),(x2+xshift,y2+xshift))
      end
    | ScaleDown(sd, (xscaled,yscaled)) =>
      let 
        val ((x1,y1),(x2,y2)) = boundingbox(sd)
        val width = x2-x1
        val height = y2-y1
      in
        ((x1,y1),(x1 + (width div xscaled),y1+(height div yscaled)))
      end
    | ScaleUp(su, (xscaleu,yscaleu)) => 
      let 
        val ((x1,y1),(x2,y2)) = boundingbox(su)
        val width = x2-x1
        val height = y2-y1
      in
        ((x1,y1),(x1 + (width * xscaleu),y1+(height * yscaleu)))
      end
    | Union(s1,s2) => 
        let
          val ((x1,y1),(x2,y2)) = boundingbox(s1)
          val ((z1,w1),(z2,w2)) = boundingbox(s2)
        in
          ((min(x1,z1),min(y1,w1)),(max(x2,z2),max(y2,w2)))
        end



val bowtie = Union(Rect((0,0),(100,100)),
                   Union(Rect((100,100),(200,200)),
                              Disc((100,100),40)))
(* to test before you've implemented the bounding box, do 
   writeshape(200,200, bowtie,"output2.bmp");
*)

val example = 
    let val b = Rect((100,50),(250,150))
        val t = Rect((250,120),(375,135))
        val h = Disc((100,175),60)
        val es = Rect((75,205),(125,250))
        val le = Disc((75,175),5)
        val re = Disc((125,175),5)
        val lre = Union(le, re)
    in 
        Union(Union(b, t),
              Without(h, Union(es,lre)))
    end
(* to test, do 
   writeshape(415,285, example,"output2.bmp");
*)

(* Other Tests *)

val lump = 
let
  val move = Rect((100,100),(200,200))
in Union(Rect((0,0),(100,100)),
                   Union(Translate(move,(0,~100)),
                              Disc((100,100),40)))
end

val egg = 
  let
    val circle = Disc((25,25),25)
  in
    ScaleUp(circle, (1,3))
  end

val pancake = 
  let
    val circle = Disc((75,75),75)
  in
    ScaleDown(circle, (2,3))
  end

val true = contains bowtie (100,100) 
val false = contains bowtie (0,200)
val false = contains egg (50,150)

val ((0,0),(200,200)) = boundingbox(bowtie)
val ((0,0),(50,150)) = boundingbox(egg) 



(* rectangle border with r as the inside border *)
fun rectb (r as ((minx,miny) : point, (maxx,maxy) : point),
           thickness : int) = 
    Without (Rect ((minx - thickness, miny - thickness),
                   (maxx + thickness, maxy + thickness)),
             Rect r)
(* 
use this to text boundingbox
val example_bb = Union (example , rectb(boundingbox example,25)) 
*)


val sierptri_box = Rect((0,0),(512,512))
val sierptri_example = ScaleUp(example,(8,8))
val sierptri_example2 = ScaleUp(Union(example,rectb(((40,50),(375,385)),4)),(2,2))

(* TASK *)
fun sierptri (base : shape) (n : int) : shape = 
  case n of
    0 => base 
    | _ => let
      val ((x1,y1),(x2,y2)) = boundingbox(base)
    in
      ScaleDown((sierptri (Union(base, Union( 
          Translate(base,(x2-x1,0)),
          Translate(base,( (x2-x1) div 2, y2-y1))) 
        )
        )
        (n-1)), (2,2)) 
    end


(* BONUS *)

fun sierpcarpet(n : int): shape =
  case n of 
    0 => Rect((0,0),(512,512))
    | n => let
      val ((x1,y1),(x2,y2)) = boundingbox(Rect((0,0),(512,512)))
      val base = sierpcarpet(n-1)
    in
       ScaleDown(Union
        (Union
          (Union(base,Translate(base,(0,512))),
            Union(Translate(base,(0,1024)),Translate(base,(512,1024)))),
          Union
          (Union(Translate(base,(1024,1024)),Translate(base,(1024,512))),
           Union(Translate(base,(1024,0)),Translate(base,(512,0))))), 

        (3,3))
      end 


(* prints the points (x,y) such that x is in [0,width] and y is in [0,height] *)
fun writeshape (width : int, height : int, s : shape, filename : string) : unit = 
    write_bitmap (width,height, contains s, filename) 
        
(* assumes that the all points in the shape are non-negative (otherwise you can Translate) *)
fun writeshape_bb (s : shape, filename : string) : unit = 
    let val ((minx,miny),(maxx,maxy)) = boundingbox s
    in 
        writeshape (maxx+minx,maxy+miny,s,filename) 
    end


