(ns pterodactyl.phalange
  (:require [taoensso.truss :as truss :refer (have have! have?)])
  (:gen-class))

(defprotocol Piece
  (piece-length [p])
  (piece-string [p])
  (split-piece [p at]))

(defrecord StringPiece [string from to]
  Piece
  (piece-length [piece]
    (- (:to piece) (:from piece)))

  (piece-string [piece]
    (let [{:keys [:string :from :to]} piece]
      (subs string from to)))

  (split-piece [piece at]
    {:pre [(<= 0 at)
           (< at (piece-length piece))]}
    (let [length (piece-length piece)]
      (if (zero? at)
        [piece]
        (let [pivot (+ at (:from piece))
              before (assoc piece :to pivot)
              after (assoc piece :from pivot)]
         [before after])))))

(defn make-string-piece [string]
  {:pre [(string? string)]}
  (StringPiece. string 0 (count string)))

(def END-OF-BUFFER
  (reify
    Object
    (toString [_] "<END-OF-BUFFER>")
    Piece
    (piece-length [_] 1)
    (piece-string [_] "")
    (split-piece [p _] [p])))

(defrecord Table [pieces])
(def table? (partial instance? Table))
(defn make-table [strings]
  {:pre [(every? string? strings)]}
  (Table. (concat (map make-string-piece strings) [END-OF-BUFFER])))

(defn show-table [table]
  {:pre [(table? table)]}
  (apply str (map piece-string (:pieces table))))
         
; zipper class, a finger onto the data
; (Clojure has zippers, but they seem to be only on hierarchical data
; structures?)
; NB: the accumulator will probably become a record, rather than a single pos
(defrecord Dactyl [back pieces acc-pos curr-pos])

(def dactyl? (partial instance? Dactyl))

(def empty-dactyl
  (Dactyl. '() '() 0 0))

(defn make-dactyl [table]
  {:pre [(table? table)]}
  (assoc empty-dactyl :pieces (:pieces table)))

(defn curr [dactyl]
  {:pre [(dactyl? dactyl)]}
  (first (:pieces dactyl)))

(defn curr-text [dactyl]
  {:pre [(dactyl? dactyl)]}
  (piece-string (curr dactyl)))

(defn curr-text-post [dactyl]
  {:pre [(dactyl? dactyl)]}
  (subs (curr-text dactyl) (:curr-pos dactyl)))

(defn curr-text-pre [dactyl]
  {:pre [(dactyl? dactyl)]}
  (subs (curr-text dactyl) 0 (:curr-pos dactyl)))

(defn curr-pos-post [dactyl]
  {:pre [(dactyl? dactyl)]}
  "Number of characters from curr-pos to end of piece (dual of curr-pos)"
  (- (piece-length (curr dactyl)) (:curr-pos dactyl)))

(defn dactyl-pos [dactyl]
  {:pre [(dactyl? dactyl)]}
  (+ (:acc-pos dactyl) (:curr-pos dactyl)))

(defn traverse-back [dactyl]
  {:pre [(dactyl? dactyl)]}
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
  {:pre [(dactyl? dactyl)]}
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
  {:pre [(dactyl? dactyl)
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
  {:pre [(dactyl? dactyl)
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
   {:pre [(dactyl? dactyl)
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

(defn till [dactyl dir string]
  {:pre [(dactyl? dactyl)
         (string? string)]}
  (let [nudge (have (dir {:left nudge-left, :right nudge-right}))
        length (count string)]
    (loop [d dactyl]
      (let [d' (nudge d)]
        (if (or 
              (= string (text-after d' length))
              (:bounce d'))
            d'
            (recur d'))))))

(def right-till #(till %1 :right %2))
(def left-till  #(till %1 :left %2))

(defn goto [dactyl pos]
  {:pre [(dactyl? dactyl)
         (<= 0 pos)]}
  (let [curr-pos (dactyl-pos dactyl)]
    (cond
      (= pos curr-pos) dactyl
      (> pos curr-pos) (traverse-right dactyl (- pos curr-pos))
      (< pos curr-pos) (traverse-left dactyl (- curr-pos pos)))))

; mostly cargo culted from (some->)
(defmacro =>
  "When expr is not :bounce, threads it into the first form (via ->),
  and when that result is not :bounce, through the next etc"
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [step] `(if (:bounce ~g) ~g (-> ~g ~step)))
                   forms)]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defn go-start-of-line [dactyl]
  (=> dactyl
      (left-till "\n")
      (nudge-right)))

(defn go-end-of-line [dactyl]
  (=> dactyl
      (right-till "\n")))

(defn go-start-of-prev-line [dactyl]
  (=> dactyl
      (go-start-of-line)
      (nudge-left)
      (go-start-of-line)))

(defn go-start-of-next-line [dactyl]
  (=> dactyl
      (go-end-of-line)
      (nudge-right)))

(defn dactyl-delta [d1 d2]
  (apply - (map dactyl-pos [d2 d1])))

(defn col-pos [dactyl]
  (let [start (go-start-of-line dactyl)]
    (dactyl-delta start dactyl)))

(defn go-col [dactyl col]
  {:pre [(dactyl? dactyl)
         (<= 0 col)]}
  (let [current-col (col-pos dactyl)
        eol (go-end-of-line dactyl)
        delta (- col current-col)]
    (cond
      (= col current-col) dactyl
      (> col (col-pos eol)) eol
      (> col current-col) (traverse-right (dactyl delta))
      (< col current-col) (traverse-left  (dactyl (- delta))))))

(defn split-dactyl [dactyl]
  {:pre [(dactyl? dactyl)]
   :post [dactyl?]}
  "Split the dactyl at current insertion point.  Anything to left of curr-pos
  will become a new piece.  Noop if we are at far-left of piece."
  (let [{:keys [curr-pos back pieces acc-pos]} dactyl]
    (if (zero? curr-pos)
      dactyl
      (let [[pre post] (split-piece (curr dactyl) curr-pos)]
        ; tempted to use (update) instead, e.g. with
        ;   (comp (partial cons post) rest)
        ; but not sure that's any more readable...
        (assoc dactyl
               :back (conj back pre)
               :pieces (conj (rest pieces) post)
               :curr-pos 0
               :acc-pos (dactyl-pos dactyl))))))

(defn cut [dactyl movement]
  {:pre [(dactyl? dactyl)
         (fn? movement)]}
  ;; split both origin and end point, and make sure that both dactyls
  ;; have both splits in their piece lists by moving *back* to origin
  ;; this allows us to take-while identical? instead of having to do
  ;; any more complicated calculation.
  (let [dactyl (split-dactyl dactyl)
        other (split-dactyl (have dactyl? (movement dactyl)))
        dactyl (goto other (dactyl-pos dactyl))
        [d1 d2] (sort-by dactyl-pos [dactyl other])
        target (first (:pieces d2))
        cut-pieces (vec (take-while #(not (identical? % target)) (:pieces d1))) 
        deleted-dactyl (assoc d1 :pieces (:pieces d2))]
    [deleted-dactyl cut-pieces])) 

(def delete-to (comp first cut))
(def copy-range (comp ->Table second cut))

(defn insert [dactyl string]
  {:pre [(dactyl? dactyl)
         (string? string)]
   :post [dactyl?]}
  (let [piece (make-string-piece string)
        dactyl (split-dactyl dactyl)]
    (update dactyl :pieces (partial cons piece))))

; next steps
  ; remove Table abstraction (is just a list of Pieces)
  ; rename Dactyl -> Phalange
  ; protocol for Phalange
  ; convenience functions for whole buffer (from start / point)
  ; marks
    ;; go to mark
    ;; (does this require storage within each piece?  or just phalange?)
  ; accumulators
    ;; probably requires storage in each piece?
    ;; go to line
    ;; go up/down (strictly doesn't need accumulator)
  ; text-before?
