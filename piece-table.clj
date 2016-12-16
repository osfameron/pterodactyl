(defrecord Piece [string from to])
(defn make-piece [string]
  (Piece. string 0 (count string)))

(defn piece-length [piece] (- (:to piece) (:from piece)))

(defn split-piece [piece at]
  (let [length (piece-length piece)]
    (cond 
      (<= at 0) [piece]
      (>= at length) [piece] ; but this shouldn't happen, except at end of buffer
      :else (let [pivot (+ at (:from piece))
                  before (assoc piece :to pivot)
                  after (assoc piece :from pivot)]
               [before after]))))

(defrecord Table [strings pieces])
(defn make-table [strings]
  (Table. strings 
          (into '() (reverse 
                      (map make-piece strings)))))

(defn show-piece [strings piece]
  (let [{:keys [string from to]} piece]
      (subs string from to)))

(defn show-table [table]
  (let [{:keys [:pieces :strings]} table
        show-piece' (partial show-piece strings)]
    (apply str (map show-piece' pieces))))
         
; zipper class, a finger onto the data
; (Clojure has zippers, but they seem to be only on hierarchical data
; structures?)
(defrecord Dactyl [strings back pieces acc-pos curr-pos])
(defn make-dactyl [table]
  (Dactyl. (:strings table) '() (:pieces table) 0 0))

(defn curr [dactyl]
  (first (:pieces dactyl)))

(defn curr-text [dactyl]
  (let [piece (curr dactyl)
        {:keys [from to]} piece
        {:keys [strings curr-pos]} dactyl]
    (subs string from to)))

(defn curr-text-post [dactyl]
  (subs (curr-text dactyl) (:curr-pos dactyl)))

(defn curr-text-pre [dactyl]
  (subs (curr-text dactyl) 0 (:curr-pos dactyl)))

(defn curr-pos-post [dactyl]
  "Number of characters from curr-pos to end of piece (dual of curr-pos)"
  (- (piece-length (curr dactyl)) (:curr-pos dactyl)))

(defn dactyl-pos [dactyl]
  (+ (:acc-pos dactyl) (:curr-pos dactyl)))

(defn traverse-back [dactyl]
  (let [{:keys [:back :pieces :acc-pos]} dactyl]
    (if (empty? back)
      nil
      (let [new (first back)
            length (piece-length new)]
        (assoc dactyl
               :back (rest back)
               :pieces (conj pieces new)
               :acc-pos (- acc-pos length)
               :curr-pos length)))))

(defn traverse-forward [dactyl]
  (let [{:keys [:back :pieces :acc-pos]} dactyl
        next (rest pieces)]
    (if (empty? next)
      nil
      (let [old (first pieces)]
        (assoc dactyl
               :back (conj back old)
               :pieces next
               :acc-pos (+ acc-pos (piece-length old)) 
               :curr-pos 0)))))

; For now, we'll clear the bounce flag at the beginning of a traverse
; I'm assuming this is a noop if already not set - e.g. won't create
; new datastructure?  In any case, this will probably move to outer
; controller framework eventually.
(defn traverse-right [dactyl count]
  (let [dactyl (dissoc dactyl :bounce) 
        avail (curr-pos-post dactyl)]
    (cond
      (zero? count) dactyl
      (< count avail) (update dactyl :curr-pos (partial + count))
      :else 
        (let [next (traverse-forward dactyl)]
          (if (nil? next)
            (assoc dactyl :bounce :right, :curr-pos (piece-length (curr dactyl)))
            (recur next (- count avail)))))))

(defn traverse-left [dactyl count]
  (let [dactyl (dissoc dactyl :bounce) 
        avail (:curr-pos dactyl)]
    (cond
      (zero? count) dactyl
      (<= count avail) (update dactyl :curr-pos #(- % count))
      :else 
        (let [next (traverse-back dactyl)]
          (if (nil? next)
            (assoc dactyl :bounce :left, :curr-pos 0)
            (recur next (- count avail)))))))
   
(defn text-after
  ([dactyl len] 
   (apply str (text-after dactyl len [])))

  ([dactyl len acc]
   (cond
     (nil? dactyl) acc
     (zero? len) acc
     :else
      (let [text (curr-text-post dactyl)
            chunk (subs text 0 (min len (count text)))]
        (recur
          (traverse-forward dactyl)
          (- len (count chunk))
          (conj acc chunk)))))) 

(defn split-dactyl [dactyl]
  "Split the dactyl at current insertion point.  Anything to left of curr-pos will become a new piece.  Noop if we are at far-left of piece."
  (let [{:keys [curr-pos back pieces acc-pos]} dactyl]
    (if (zero? curr-pos)
      dactyl
      (let [[pre post] (split-piece (curr dactyl) curr-pos)]
        (assoc dactyl
               :back (conj back pre)
               :pieces (conj (rest pieces) post)
               :curr-pos 0
               :acc-pos (+ acc-pos (count pre)))))))

(defn delete-between [d1 d2]
  (if (> (dactyl-pos d1) (dactyl-pos d2))
    (delete-between d2 d1)
    (let [d1' (split-dactyl d1)
          d2' (split-dactyl d2)]
      (assoc d1' :pieces (:pieces d2')))))
     
; next steps
  ; insert
  ; text-before?
  ; go to
  ; unzip & print whole buffer

(comment
  (def table (make-table ["hello " "world"]))
  (show-table table)
  (def d (make-dactyl table))
  (text-after d 100)
  (doseq [i (range 12)] 
    (-> d (traverse-right i)
          (#(println (str)
             (-> % (:curr-pos))
             " / "
             (-> % (curr) (:string))
             " / "
             (-> % (dactyl-pos))
             " / "
             (-> % (text-after 100)))))) 
  (doseq [i (range 12)] 
    (-> (traverse-right d 11) (traverse-left i)
          (#(println (str)
             (-> % (:curr-pos))
             " / "
             (-> % (curr) (:string))
             " / "
             (-> % (dactyl-pos))
             " / "
             (-> % (text-after 100)))))) 

  (-> d (traverse-right 6) (text-after 100))
  (-> d (traverse-right 3) (split-dactyl) (:curr-pos))
  (-> d (traverse-right 3) (split-dactyl) (curr-text))
  (-> d (traverse-right 3) (text-after 100))
  (-> d (traverse-right 3) (split-dactyl) (text-after 100))
  (-> d (traverse-right 10) 
      (#(let [d1 %
              d2 (traverse-left d1 4)]
         (delete-between d1 d2)))
      (traverse-left 100)
      (text-after 100))

  (-> d (traverse-right 6) (text-after 100))
  (-> d (traverse-right 7))
  (-> d (traverse-right 7) (traverse-left 2) (text-after 100))
  (-> d (traverse-right 7) (traverse-left 2))
  (traverse-forward (traverse-right d 1))
  (:acc-pos (traverse-forward d))
  (:acc-pos (traverse-back (traverse-forward d)))
  (:acc-pos (traverse-forward (traverse-forward (traverse-forward d)))))
