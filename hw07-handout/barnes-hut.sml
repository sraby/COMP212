structure BarnesHut =
struct

  open Mechanics
  structure BB = BoundingBox
  open Plane
  open TestData 

  infixr 3 ++
  infixr 4 **
  infixr 3 --> 

  datatype bhtree =
      Empty
    | Single of body
    | Cell of (Scalar.scalar * Plane.point) * BB.bbox * bhtree * bhtree * bhtree * bhtree
      (* ((mass, center), box, top-left, top-right, bottom-left, bottom-right) *)

  (* Projects the mass and center from the root node of a bhtree *)
  fun center_of_mass (T : bhtree) : Scalar.scalar * Plane.point =
      case T of
          Empty => (Scalar.zero, Plane.origin)
        | Single (m, p, _) => (m, p)
        | Cell (com, _, _,_,_,_) => com

  (* Note: Doesn't compare velocities as these are unaffected by compute_tree *)
  fun bodyEq ((m1, p1, _) : body, (m2, p2, _) : body) : bool =
      (Scalar.eq (m1, m2)) andalso Plane.pointEqual (p1, p2)

  fun bhtreeEq (t1 : bhtree, t2 : bhtree) : bool =
      case (t1, t2) of
          (Empty, Empty) => true
        | (Single b1, Single b2) => bodyEq (b1, b2)
        | (Cell ((cm1, cp1), bb1, tl1,tr1,bl1,br1), Cell ((cm2, cp2), bb2, tl2,tr2,bl2,br2)) =>
              Scalar.eq (cm1, cm2) andalso
              Plane.pointEqual (cp1, cp2) andalso
              BB.equal (bb1, bb2) andalso 
              bhtreeEq (tl1,tl2) andalso bhtreeEq (tr1,tr2) andalso 
              bhtreeEq (bl1,bl2) andalso bhtreeEq (br1,br2)
        | (_, _) => false

  (* ---------------------------------------------------------------------- *)
  (* TASKS *)

  (* TASK *)
  (* Purpose: Computes a pair (m,c) such that m is the total mass of the 4 bodies, 
  and c is the barycenter of the 4 bodies

    Example: val (Scalar.fromRatio(4,1),(Scalar.fromRatio(2,1),Scalar.fromRatio(2,1))) = 
        barycenter((Scalar.fromRatio(1,1),(Scalar.fromRatio(1,1),Scalar.fromRatio(1,1))),
                  (Scalar.fromRatio(1,1),(Scalar.fromRatio(3,1),Scalar.fromRatio(1,1))),
                  (Scalar.fromRatio(1,1),(Scalar.fromRatio(3,1),Scalar.fromRatio(3,1))),
                  (Scalar.fromRatio(1,1),(Scalar.fromRatio(1,1),Scalar.fromRatio(3,1))))

   *)
  (* Assumes the total mass of the points is positive *)
  fun barycenter ((m1,p1) : (Scalar.scalar * Plane.point),
                  (m2,p2) : (Scalar.scalar * Plane.point),
                  (m3,p3) : (Scalar.scalar * Plane.point),
                  (m4,p4) : (Scalar.scalar * Plane.point)) : Scalar.scalar * Plane.point =
     let
       val m = Scalar.plus(Scalar.plus(m1,m2), Scalar.plus(m3,m4)) 
       val mdenom = Scalar.divide(Scalar.fromRatio(1,1), m)
       val c = head((((origin-->p1)**m1)++((origin-->p2)**m2)++((origin-->p3)**m3)++((origin-->p4)**m4))**mdenom)
     in
       (m,c)
     end



  (* TASK *)
  (* Purpose: quarters(bb) returns the four quadrants of bb 
      Example: val true = let val (tl,tr,bl,br) = quarters(bb4) 
             in BB.equal(tl,bb0) andalso BB.equal(tr,bb1) andalso
                BB.equal(bl, bb2) andalso BB.equal(br,bb3)
             end

    *)
  fun quarters (bb : BB.bbox) : BB.bbox * BB.bbox * BB.bbox * BB.bbox =
    let
      val (tl,tr,bl,br) = BB.corners(bb)
      val center = BB.center(bb)
    in
      (BB.from2Points(tl,center),BB.from2Points(tr,center),BB.from2Points(bl,center),
        BB.from2Points(br,center))
    end
      

  val true = let val (tl,tr,bl,br) = quarters(bb4) 
             in BB.equal(tl,bb0) andalso BB.equal(tr,bb1) andalso
                BB.equal(bl,bb2) andalso BB.equal(br,bb3)
             end


  (* TASK *)
  (* Purpose: cumpute_tree s bb computes the Barnes-Hut tree for the bodies in s contained in bb.
   * Assumes all bodies are contained in bb,
     and that no two bodies have collided (or are so close that dividing the 
     bounding box will not eventually separate them).
     *)


  fun compute_tree (s : body Seq.seq) (bb : BB.bbox) : bhtree = 
    case Seq.length s of 
      0 => Empty
      | 1 => Single(Seq.nth 0 s)
      | _ => let
        val (tlbb,trbb,blbb,brbb) = quarters(bb)
        val tlseq = Seq.filter (fn (_,p,_) => BB.contained (false,false,false,false) (p,tlbb)) s 
        val trseq = Seq.filter (fn (_,p,_) => BB.contained (true,false,false,false) (p,trbb)) s
        val blseq = Seq.filter (fn (_,p,_) => BB.contained (false,false,true,false) (p,blbb)) s
        val brseq = Seq.filter (fn (_,p,_) => BB.contained (true,false,true,false) (p,brbb)) s 
        val t1 = compute_tree tlseq tlbb
        val t2 = compute_tree trseq trbb
        val t3 = compute_tree blseq blbb 
        val t4 = compute_tree brseq brbb
      in
        Cell(barycenter(center_of_mass(t1),center_of_mass(t2),center_of_mass(t3),center_of_mass(t4)),
          bb, 
          t1,t2,t3,t4 )
      end

(* Test *)

  val three_bodies = Seq.cons body1 (Seq.cons body2 (Seq.cons body3 (Seq.empty())))
  val three_bodies_tree = Cell ((Scalar.fromInt 3, p22), bb4,
                                Cell ((Scalar.fromInt 2, p13), bb0,
                                      Single body3, Empty, Empty, Single body2), 
                                Empty, 
                                Empty, 
                                Single body1)
  val true = bhtreeEq (compute_tree three_bodies bb4, three_bodies_tree) 


  (* TASK *)
  (* too_far p1 p2 bb t determines if point p1 is "too far" from 
   * a region bb with barycenter p2, given a threshold parameter t,
   * for it to be worth recuring into the region
   *)
  fun too_far (p1 : Plane.point) (p2 : Plane.point) (bb : BB.bbox) (t : Scalar.scalar) : bool =
      let
        val diam:Scalar.scalar = BB.diameter(bb)
        val dist:Scalar.scalar = distance p1 p2 
        val ratio:Scalar.scalar = Scalar.divide(diam,dist)
      in
        case Scalar.compare(ratio,t) of 
          GREATER => false 
          | _ => true
      end

  (* TASK *)
  (* Computes the acceleration on b from the tree T using the Barnes-Hut
   * algorithm with threshold t
   *)
  

  fun bh_acceleration (T : bhtree) (t : Scalar.scalar) (b : body) : Plane.vec = 
      let val (_,p,_) = b in
        case T of 
          Empty => zero
          | Single(b2) => accOn(b,b2) 
          | Cell((mass,center), box,t1,t2,t3,t4) => case (too_far p center box t) of
                                                        true => accOn(b, (mass,center,zero))
                                                        | false => (bh_acceleration t1 t b)++
                                                                              (bh_acceleration t2 t b)++
                                                                  (bh_acceleration t3 t b)++
                                                                              (bh_acceleration t4 t b)
  end 
      

  (* TASK
     Given a threshold and a sequence of bodies, compute the acceleration
     on each body using the Barnes-Hut algorithm.
   *)
  fun barnes_hut (threshold : Scalar.scalar) (s : body Seq.seq) : Plane.vec Seq.seq = 
      let
        val pointseq = Seq.map (fn (m,c,v) => c) s 
        val bb = BB.fromPoints pointseq
        val tree = compute_tree s bb
      in
        Seq.tabulate (fn i => bh_acceleration tree threshold (Seq.nth i s)) (Seq.length s)
      end

  (* Default value of the threshold, theta = 0.5 *)
  val threshold = (Scalar.fromRatio (1,2))

  val accelerations : body Seq.seq -> Plane.vec Seq.seq = barnes_hut threshold

end
