(defrecord Piece [table-index from to])
(defn make-piece [table-index string]
  (Piece. table-index 0 (count string)))

(defn piece-length [piece] (- (:to piece) (:from piece)))

(defn split-piece [piece at]
  (let [length (piece-length piece)]
    (cond 
      (<= at 0) [piece]
      (>= at length) [piece]
      :else (let [pivot (+ at (:from piece))
                  before (assoc piece :to pivot)
                  after (assoc piece :from pivot)]
               [before after]))))

(defrecord Table [strings pieces])
(defn make-table [strings]
  (Table. strings 
          (into '() (reverse 
                      (map make-piece (range) strings)))))

(defn show-piece [strings piece]
  (let [{:keys [table-index from to]} piece
        string (nth strings table-index)]
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
  (nth (:strings dactyl) (:table-index (curr dactyl))))

(defn curr-text-post [dactyl]
  (subs (curr-text dactyl) (:curr-pos dactyl)))

(defn curr-text-pre [dactyl]
  (subs (curr-text dactyl) 0 (:curr-pos dactyl)))

(defn curr-post [dactyl]
  (- (piece-length (curr dactyl)) (:curr-pos dactyl)))

(defn traverse-back [dactyl]
  (let [{:keys [:back :pieces :acc-pos]} dactyl]
    (if (empty? back)
      nil
      (let [new (first back)]
        (assoc dactyl
               :back (rest back)
               :pieces (conj pieces new)
               :acc-pos (- acc-pos (piece-length new)) 
               :curr-pos 0)))))

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

(defn traverse-right [dactyl count]
  (let [dactyl (dissoc dactyl :bounce) ; assuming this will be cheap noop in
                                       ; most cases?
        avail (curr-post dactyl)]
    (cond
      (zero? count) dactyl
      (< count avail) (update dactyl :curr-pos (partial + count))
      :else 
        (let [next (traverse-forward dactyl)]
          (if (nil? next)
            (assoc dactyl :bounce :right, :curr-pos (piece-length (curr dactyl)))
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
     

(comment
  (def table (make-table ["hello " "world"]))
  (show-table table)
  (def d (make-dactyl table))
  (text-after d 100)
  (text-after (traverse-right d 3) 100)
  (traverse-forward (traverse-right d 1))
  (:acc-pos (traverse-forward d))
  (:acc-pos (traverse-back (traverse-back d)))
  (:acc-pos (traverse-forward (traverse-forward (traverse-forward d)))))
