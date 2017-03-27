
functor NaiveBayes (ClassSpec : sig
                                  structure Category : ORDERED
                                  val default_category : Category.t
                                      
                                  structure Dataset : MAP_REDUCE
                                end) : NAIVE_BAYES_CLASSIFIER =
struct

    type category = ClassSpec.Category.t

    type labeled_document = category Seq.seq * string Seq.seq
    type document = string Seq.seq

    structure Dataset = ClassSpec.Dataset
        
    (* TASK 
       make the CatEC and CatDict and WordDict modules here
       *)
    structure WordDict = Dict(StringLt)
    structure CatEC = ExtractCombine(struct 
                                structure Key = ClassSpec.Category
                                structure MR = ClassSpec.Dataset
                            end)
    structure CatDict = CatEC.D
    
    type counts = 
        int (* number of labeled_documents with that category *)
      * int WordDict.dict (* frequencies of words in labeled_documents with that category *)

    fun counts_documents ((n,_) : counts) : int = n
    fun counts_words     ((_,w) : counts) : int WordDict.dict = w


    (* TASK *)
    fun count_by_category (docs : labeled_document Dataset.mapreducable) : counts CatDict.dict =
        let
          val combiner = 
                (fn((n1,d1),(n2,d2)) => (n1 + n2, WordDict.merge Int.+ (d1,d2)))
          val extractor = 
                (fn(catseq,sseq) => Seq.map (fn(cat) => (cat,
                  (1, Seq.mapreduce (fn(s) => WordDict.insert WordDict.empty (s,1)) 
                    (WordDict.empty) 
                    (WordDict.merge Int.+) 
                    (sseq)))) catseq)
        in
          CatEC.extractcombine extractor combiner docs 
        end

    type postprocess_data =
          category Seq.seq (* list of categories (no duplicates) *)
        * int              (* total number of categorized training labeled_documents (count doc once for each label) *)
        * int              (* total number of words *)
        * int CatDict.dict (* how many words in each category? *) 

    (* TASK *)
    (* I implemented postprocess using the solution key posted on Piazza :( *)
    fun postprocess (counts_by_category : counts CatDict.dict) : postprocess_data = 
      let 
            val counts_seq = CatDict.toSeq counts_by_category
            val all_categories = Seq.map (fn (c,_) => c) counts_seq

            val total_num_docs = Seq.mapreduce (fn (_,(ccount,_)) => ccount) 0 Int.+ counts_seq

            val num_words_by_cat = 
                CatDict.map (fn (_,wordfreqs) => Seq.reduce Int.+ 0 (WordDict.valueSeq wordfreqs)) counts_by_category

            val total_num_words = 
                (Seq.length (WordDict.toSeq (Seq.mapreduce (fn (_,(_,wd)) => wd)
                                              WordDict.empty
                                              (WordDict.merge (fn (_,_) => 0)) (* don't care about counts *) 
                                              counts_seq)))
        in 
            (all_categories, 
             total_num_docs,
             total_num_words,  
             num_words_by_cat)
        end  
        
    (* TASK *)
    fun possible_classifications 
        (counts_by_category : counts CatDict.dict)
        ((all_categories, total_num_docs, total_num_words, num_words_by_cat) : postprocess_data)
        (test_doc : document) : (category * real) Seq.seq =
        Seq.map (fn(C) => 
          let
            val numdocinC = case CatDict.lookup counts_by_category C of
                                        NONE => 0
                                        | SOME(x,_) => x 
            val part1 = Math.ln( (Real.fromInt numdocinC)/(Real.fromInt total_num_docs) )
            val part2 = Seq.mapreduce (fn(word) =>
                    case WordDict.lookup (counts_words(CatDict.lookup' counts_by_category C)) word of 
                                        NONE => Math.ln(1.0/(Real.fromInt(total_num_words)))
                                        | SOME(x) => Math.ln(Real.fromInt(x)/(Real.fromInt(CatDict.lookup' num_words_by_cat C)))
                )
                0.0 
                Real.+ 
                test_doc
          in
            (C, Real.+(part1,part2))
          end 

          ) (all_categories) 

    (* TASK *)

    (* I implemented classify without looking at the solutions! *)
    fun classify (counts_by_category : counts CatDict.dict)
                 (pp : postprocess_data)
                 (test_doc : document) : (category * real) =
                 let val freqseq = possible_classifications counts_by_category pp test_doc 
                 in 
                    Seq.mapreduce 
                    (fn(a) => a)
                    (ClassSpec.default_category, Real.negInf)
                    (fn((c1,f1),(c2,f2)) => 
                        case Real.compare(f1,f2) of
                          LESS => (c2,f2)
                          | GREATER => (c1,f1)
                          | EQUAL => (c1,f1) )
                    freqseq 
                 end 

    (* TASK *)
    (* I implemented train_classifier without looking at the solutions! *)
    fun train_classifier (train : labeled_document Dataset.mapreducable) : document -> (category * real) =
        let
          val cbc = count_by_category train
          val pp = postprocess cbc 
        in
          classify cbc pp 
        end

    (* TASK report on the percentage correct in a comment here:
        
        This classifier run using small_train.txt on small_test.txt got 6/8 classifications correct,
            which is 75% correct.

        Run using medium_train.txt on medium_test.txt, we got 735/818 classifications correct
            which is about 89.9% correct. 

        Run using big_train.txt on big_test.txt, we got 74131/80435 classifications correct,
            which is about 92.2% correct. 

       *** Run using medium_train.txt on small_test.txt, we got 8/8 classifications correct,
            which is 100% correct! 

       *** This shows how additional training data can have big improvements on
          our classificaton results. 
     *)
end
