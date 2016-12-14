(defrecord Table [strings pieces])

(defrecord Piece [table-index from to])

(defn piece-length [piece] (- (:to piece) (:from piece)))

(defn make-piece [string table-index]
  (Piece. table-index 0 (count string)))

(defn make-table [strings]
  (Table. strings 
          (into '() (reverse 
                      (map make-piece strings (range))))))

(defn show-table [table]
  (let [ strings (:strings table)
         pieces (:pieces table)
         show-piece (fn [piece]
                      (let [ table-index (:table-index piece)
                             from (:from piece)
                             to (:to piece)
                             string (get strings table-index)]
                          (subs string from to)))]
    (apply str (map show-piece pieces))))
    
(defn pieces-with-offset [pieces]
  "Returns a vector like [11 [ [0 6 piece1] [6 11 piece2] ] ]
   e.g. [length [ [from to piece1] [from to piece2] ] ]"
  (reduce 
    (fn [a b] 
      (let [from (first a)
            acc (second a)
            to (+ from (piece-length b))
            piece [from to b]]
        [ to (conj acc piece)]))
    [0 []]
    pieces))

(defn split-pieces [pieces at]
  (let [pwo (pieces-with-offset pieces)
        length (first pwo)
        pwos (second pwo)
        rough (split-with #(< (first %) at) pwos)
        pre (into [] (first rough))
        pivot (last pre)
        pre' (pop pre)
        post (second rough)
        pivot-from (first pivot)
        split-offset (- at pivot-from)
        pp (last pivot)
        table-offset (+ (:from pp) split-offset)
        pivot-pre (Piece. (:table-index pp) (:from pp) table-offset) 
        pivot-post (Piece. (:table-index pp) split-offset (:to pp))
        fc (fn [a b]
             (filter #(> (piece-length %) 0)
               (concat a b)))]
     [ (fc (into [] (map last pre')) [pivot-pre])
       (fc [pivot-post] (into [] (map last post)))]))
  
(defn split-table [table at]
  "Largely useless, except for testing?"
  (Table. (:strings table) (apply concat (split-pieces (:pieces table) at)))) 
  
(defn table-delete [table from to]
  "NB: from < to, or nonsensical results, TODO"
  (let [pieces (:pieces table) 
        pre (split-pieces pieces from)
        post (split-pieces pieces to)
        left (first pre)
        right (second post)]
    (Table. (:strings table) (concat left right))))
         
   

(comment
  (concat (into [] (concat '((1)) '((2)))))
  (pop [1 2 3])
  (last '(1 2 3 4))
  (into [] (list 1 2 3 4))
  (def table (make-table ["hello " "world"]))
  (show-table user/table)
  (> 6 (first [4 5]))
  flatmap
  (print [ table (split-table table 6)])
  (show-table table)
  (show-table (split-table table 4))
  (show-table (split-table table 6))
  (show-table (split-table table 7))
  (show-table 
    (table-delete table 7 9)))
