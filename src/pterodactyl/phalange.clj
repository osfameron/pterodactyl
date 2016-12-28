(ns pterodactyl.phalange
  (:gen-class))

(defrecord Piece [string from to])
(defn make-piece [string]
  {:pre [(string? string)]}
  (Piece. string 0 (count string)))

(defn piece-length [piece]
  {:pre [(instance? Piece piece)]}
  (- (:to piece) (:from piece)))

(defn piece-string [piece]
  {:pre [(instance? Piece piece)]}
  (let [{:keys [:string :from :to]} piece]
    (subs string from to)))

(defn split-piece [piece at]
  {:pre [(instance? Piece piece)
         (<= 0 at)
         (< at (piece-length piece))]}
         
  (let [length (piece-length piece)]
    (if (zero? at) 
      [piece]
      (let [pivot (+ at (:from piece))
            before (assoc piece :to pivot)
            after (assoc piece :from pivot)]
       [before after]))))

(defrecord Table [pieces])
(defn make-table [strings]
  {:pre [(every? string? strings)]}
  (Table. (into '() (reverse 
                      (map make-piece strings)))))

(defn show-table [table]
  {:pre [(instance? Table table)]}
  (let [{:keys [:pieces :strings]} table]
    (apply str (map piece-string pieces))))
         
; zipper class, a finger onto the data
; (Clojure has zippers, but they seem to be only on hierarchical data
; structures?)
; NB: the accumulator will probably become a record, rather than a single pos
(defrecord Dactyl [back pieces acc-pos curr-pos])

(def empty-dactyl
  (Dactyl. '() '() 0 0))

(defn make-dactyl [table]
  {:pre [(instance? Table table)]}
  (assoc empty-dactyl :pieces (:pieces table)))

(defn curr [dactyl]
  {:pre [(instance? Dactyl dactyl)]}
  (first (:pieces dactyl)))

(defn curr-text [dactyl]
  {:pre [(instance? Dactyl dactyl)]}
  (let [piece (curr dactyl)
        {:keys [string from to]} piece
        {:keys [curr-pos]} dactyl]
    (subs string from to)))

(defn curr-text-post [dactyl]
  {:pre [(instance? Dactyl dactyl)]}
  (subs (curr-text dactyl) (:curr-pos dactyl)))

(defn curr-text-pre [dactyl]
  {:pre [(instance? Dactyl dactyl)]}
  (subs (curr-text dactyl) 0 (:curr-pos dactyl)))

(defn curr-pos-post [dactyl]
  {:pre [(instance? Dactyl dactyl)]}
  "Number of characters from curr-pos to end of piece (dual of curr-pos)"
  (- (piece-length (curr dactyl)) (:curr-pos dactyl)))

(defn dactyl-pos [dactyl]
  {:pre [(instance? Dactyl dactyl)]}
  (+ (:acc-pos dactyl) (:curr-pos dactyl)))

(defn traverse-back [dactyl]
  {:pre [(instance? Dactyl dactyl)]}
  (let [{:keys [:back :pieces :acc-pos]} dactyl]
    (if (empty? back)
      nil
      (let [new (first back)
            length (piece-length new)
            last-pos (dec length)]
        (assoc dactyl
               :back (rest back)
               :pieces (conj pieces new)
               :acc-pos (- acc-pos length)
               :curr-pos last-pos)))))

(defn traverse-forward [dactyl]
  {:pre [(instance? Dactyl dactyl)]}
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
  {:pre [(instance? Dactyl dactyl)
         (<= 0 count)]}
  (let [dactyl (dissoc dactyl :bounce) 
        avail (curr-pos-post dactyl)]
    (cond
      (zero? count) dactyl
      (< count avail) (update dactyl :curr-pos (partial + count))
      :else 
        (let [next (traverse-forward dactyl)
              last-pos (dec (piece-length (curr dactyl)))]
          (if (nil? next)
            (assoc dactyl
                   :bounce :right,
                   :curr-pos last-pos)
            (recur next (- count avail)))))))

(defn traverse-left [dactyl count]
  {:pre [(instance? Dactyl dactyl)
         (<= 0 count)]}
  (let [dactyl (dissoc dactyl :bounce) 
        avail (:curr-pos dactyl)]
    (cond
      (zero? count) dactyl
      (<= count avail) (update dactyl :curr-pos #(- % count))
      :else 
        (let [next (traverse-back dactyl)
              steps-to-prev-piece (inc avail)]
          (if (nil? next)
            (assoc dactyl :bounce :left, :curr-pos 0)
            (recur next (- count steps-to-prev-piece)))))))

(defn nudge-right [dactyl]
  (traverse-right dactyl 1))

(defn nudge-left [dactyl]
  (traverse-left dactyl 1))
   
(defn text-after
  ([dactyl len] 
   {:pre [(instance? Dactyl dactyl)
          (<= 0 len)]}
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
  {:pre [(instance? Dactyl dactyl)]
   :post [(instance? Dactyl %)]}
  "Split the dactyl at current insertion point.  Anything to left of curr-pos
  will become a new piece.  Noop if we are at far-left of piece."
  (let [{:keys [curr-pos back pieces acc-pos]} dactyl]
    (if (zero? curr-pos)
      dactyl
      (let [[pre post] (split-piece (curr dactyl) curr-pos)]
        (assoc dactyl
               :back (conj back pre)
               :pieces (conj (rest pieces) post)
               :curr-pos 0
               :acc-pos (dactyl-pos dactyl))))))

(defn delete-between [d1 d2]
  (if (> (dactyl-pos d1) (dactyl-pos d2))
    (delete-between d2 d1)
    (let [d1' (split-dactyl d1)
          d2' (split-dactyl d2)]
      (assoc d1' :pieces (:pieces d2')))))

(defn -pieces-between 
  "Get the pieces between two dactyls.  These must be in order, and split"
  [left right acc]
  (let [lpos (dactyl-pos left)
        piece (curr left)
        rpos (dactyl-pos right)
        avail (curr-pos-post left)
        remaining (- rpos lpos)]
     (println (str "lpos: " lpos " rpos: " rpos " avail: " avail " rem: " remaining))
     (cond
       (nil? left)
       ("EEEEK!")

       (= lpos rpos)
       acc
       
       (<= remaining avail)
       (conj acc (assoc piece :to (+ (:from piece) remaining))) 
  
       :else
          (recur (traverse-forward left) right (conj acc piece)))))
  
(defn region-between [d1 d2]
  (let [d1pos (dactyl-pos d1)
        d2pos (dactyl-pos d2)]
    (cond
      (= d1pos d2pos)
      [d1 empty-dactyl]

      (> d1pos d2pos)
      (region-between d2 d1)

      :else
        (let [d1' (split-dactyl d1)
              d2' (split-dactyl d2)
              outer (assoc d1' 
                           :pieces (:pieces d2'))
              inner (assoc d1' 
                           :back '() 
                           :acc-pos 0 
                           :curr-pos 0 
                           :pieces (-pieces-between d1' d2' []))]
         [outer inner]))))

; next steps
  ; rename Dactyl -> Phalange
  ; test / spec
  ; end-of-buffer handling
  ; unzip & print whole buffer
  ; insert
  ; text-before?
  ; go to
  ; go to line
  ; go up/down
  ; go to mark

(comment
  (def table (make-table ["hello " "world"]))
  (show-table table)
  (def d (make-dactyl table))
  (text-after d 100)
  (doseq [i (range 12)] 
    (-> d (traverse-right i)
          (#(println (str)
             (-> % (dactyl-pos))
             "\t/ "
             (-> % (:curr-pos))
             "\t/ "
             (-> % (curr) (:string))
             "\t/ "
             "<"
             (-> % (text-after 100)) 
             ">"))))
  (doseq [i (range 12)] 
    (-> (traverse-right d 11) (traverse-left i)
          (#(println (str)
             (-> % (dactyl-pos))
             "\t/ "
             (-> % (:curr-pos))
             "\t/ "
             (-> % (curr) (:string))
             "\t/ "
             "<"
             (-> % (text-after 100)) 
             ">"))))

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

  (-> d (traverse-right 10) 
      (#(let [d1 %
              d2 (traverse-left d1 4)
              [outer inner] (region-between d1 d2)]
         (println (-> outer (traverse-left 100) (text-after 100)))
         (println (-> inner (traverse-left 100) (text-after 100))))))

  (-> d (traverse-right 8) 
      (#(let [d1 %
              d2 (traverse-left d1 4)]
          (-pieces-between (split-dactyl d2) (split-dactyl d1) []))))

  (-> d (traverse-right 6) (text-after 100))
  (-> d (traverse-right 8) (traverse-left 4) (split-dactyl) (#(println (dactyl-pos %))))
  (-> d (traverse-right 7) (traverse-left 2) (text-after 100))
  (-> d (traverse-right 7) (traverse-left 2))
  (traverse-forward (traverse-right d 1))
  (:acc-pos (traverse-forward d))
  (:acc-pos (traverse-back (traverse-forward d)))
  (:acc-pos (traverse-forward (traverse-forward (traverse-forward d)))))
