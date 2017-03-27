
functor ExtractCombine (A : sig 
                                structure Key : ORDERED
                                structure MR: MAP_REDUCE
                            end) : EXTRACT_COMBINE =
struct

	structure MR = A.MR
	structure D = Dict(A.Key)

	fun extractcombine (extract) (combine) (input) =
			(MR.mapreduce (fn(doc) => D.fromSeq(extract(doc))) 
				(D.empty) 
				(fn(d1,d2) => D.merge combine (d1,d2)) 
				input) 
end


