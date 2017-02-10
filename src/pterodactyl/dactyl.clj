(ns pterodactyl.dactyl
  (:require [net.cgrand.seqexp :as se]) 
  (:gen-class))

;; ## Pterodactyl: a programmer's text editor for winged dinosaurs

;; ### Making paired zippers

(defn pair-reductions 
  "Pair reductions"
  [acc-fn init xs]
  (map list
       xs
       (reductions acc-fn init xs)))

; zipper is of [[thing acc] [thing acc] ...]
(defn make-zipper [acc-fn init xs & [base]]
  (let [rights (pair-reductions acc-fn init xs)]
    (with-meta 
      (assoc base :right rights
                  :left nil)
      {:acc-fn acc-fn})))

(def reversed {:left :right
               :right :left}) 

;; left and right are not mirror-images... ends look like:   [1 2 3 :end]
;; so:
;;
;;   - at left, have :left nil
;;   - at right, have
;;      - :right ($end-item) e.g. 1 item, which will be either)
;;      - :end (for phalange zipper)
;;        or last-char (for dactyl zipper)
(defn end-of-zipper? [z dir]
  (let [eoz? {:left empty?
              :right (comp empty? rest)}]
    ((dir eoz?) (dir z))))

(defn traverse [z dir]
  (if (end-of-zipper? z dir)
      nil
      (let [[x & xs] (dir z)
            rev (reversed dir)]
        (assoc z dir xs
                 rev (conj (rev z) x)))))

;; ## The Accumulator

;; ### Combinators to update position within buffer

(defn pos++ [m] (update m :pos inc))
(defn row++ [m] (update m :row inc))
(defn col++ [m] (update m :col inc))
(defn col0  [m] (assoc m :col 0))
(def crlf (comp row++ col0))
(defn col-or-row++ [m c]
  (if (= \newline c)
    (crlf m)
    (col++ m)))

(def acc-init {:pos 0, :row 0, :col 0})

(defn acc-char [m c]
    (-> m
        pos++
        (col-or-row++ c)))

;; ## The Piece and the Phalange

(defn string [piece]
  (apply subs piece))

(defn length [[_ from to]]
  (- to from))

(defn string->piece [s]
  [s 0 (count s)])

(defn piece->seq [piece]
  (if (= :end piece)
      [piece]
      (seq (string piece))))

(defn make-acc-table [acc-char]
  (fn [m piece] (reduce acc-char m (piece->seq piece))))

(defn strings->phalange 
  ([strings]
   (strings->phalange strings acc-char))

  ([strings acc-fn-piece]
   (let [pieces (mapv string->piece strings)
         pieces (conj pieces :end)
         acc-fn (make-acc-table acc-fn-piece)]
     (make-zipper acc-fn
                  acc-init
                  pieces
                  {:acc-fn-piece acc-fn-piece}))))

;; ## The Dactyl

(defn phalange->dactyl [phalange]
  (let [[piece init] (first (:right phalange)) 
        xs (piece->seq piece)
        acc-fn-piece (:acc-fn-piece phalange)]
      (make-zipper acc-fn-piece
                  init
                  xs
                  {:up phalange}))) 

(defn make-dactyl [strings]
  (-> strings
      strings->phalange
      phalange->dactyl))

(defn end [dactyl]
  (let [[x xs] ((juxt last butlast) (:right dactyl))]
    (assoc dactyl :right (list x)
                  :left (reverse xs))))

(defn traverse-into [dactyl dir]
  (let [f (dir {:right identity, :left end})]
    (-> dactyl f)))


;; ### Information about the Dactyl

(defn at-char [{[[char _]] :right}]
  char)

(defn at-acc [{[[_ acc]] :right}]
  acc)

(def at-pos (comp :pos at-acc))
(def at-col (comp :col at-acc))
(def at-row (comp :row at-acc))

(defn debug [ting string] (println string) ting)
(defn debug-dactyl [dactyl]
  (debug dactyl (str "@ " (at-acc dactyl) "{" (at-char dactyl) "}"))) 

;; ### Traversals

(defn go [dactyl dir]
  (or
    (some-> dactyl
            (traverse dir))
    (some-> dactyl :up
            (traverse dir)
            phalange->dactyl
            (traverse-into dir))))

; Utility function. todo replace with unrolled version
(defn partial> [f & end-args]
  (fn [& start-args] (apply f (concat start-args end-args)))) 

(defn stream [dactyl dir]
  (->> dactyl
       (iterate (partial> go dir))
       (take-while (complement nil?))))

(defn match-char [char dactyl]
  (= char (at-char dactyl))) 

(defn find-in-stream [stream matcher repeater]
  (->> stream
       (se/exec
         (repeater (complement matcher)))
       :rest
       first))

(defn traverse-find [dactyl dir matcher & [limit]]
   (let [ds (stream dactyl dir) 
         repeater (if limit (partial se/repeat 0 limit) se/*)] 
     (or (find-in-stream ds matcher repeater)
         dactyl)))

(defn find-char [dactyl dir c & [limit]]
  (let [matcher (partial match-char c)]
    (traverse-find dactyl dir matcher limit)))

(defn go-start-of-buffer [dactyl]
  (-> dactyl
      :up
      (stream :left)
      last
      phalange->dactyl))

(defn go-to [dactyl pos]
  (let [delta (- pos (at-pos dactyl))]
    (cond
      (zero? delta) dactyl
      (pos? delta) (-> dactyl (stream :right) (nth delta))
      (neg? delta) (-> dactyl (stream :left) (nth (- delta))))))

(defn all-pos [dactyl]
  (-> dactyl
      go-start-of-buffer
      (#(map at-pos (stream % :right)))))

(defn all-text [dactyl]
  (-> dactyl
      go-start-of-buffer
      :up
      (#(apply str
               (map (comp string first)
                    (butlast (:right %)))))))

(defn go-start-of-line [dactyl]
  (-> dactyl
      (traverse-find :left (comp zero? at-col))))

(defn go-end-of-line [dactyl]
  (-> dactyl
      (find-char :right \newline)))

(defn go-to-row [dactyl row]
  (let [curr-row (at-row dactyl)
        match-row (comp (partial = row) at-row)]
    (if (= row curr-row)
      (-> dactyl go-start-of-line)
      (let [match-row (comp (partial = row) at-row)
            dir (if (> row curr-row) :right :left)]
        (-> dactyl
            (traverse-find dir match-row)))))) 

(defn go-up [dactyl]
  (let [col (at-col dactyl)
        to-col (comp (partial >= col) at-col)]
    (-> dactyl
        go-start-of-line
        (go :left)
        (traverse-find :left to-col)))) 

(defn go-down [dactyl]
  (let [col (at-col dactyl)]
    (-> dactyl
        go-end-of-line
        (go :right)
        (find-char :right \newline col))))

;; ## Splitting

(defn split-phalange [phalange split-acc]
  (let [left (:left phalange)
        [[[s from to] orig-acc] & right] (:right phalange)
        split-offset (- (:pos split-acc) (:pos orig-acc))
        length (- to from)]
    (if (< 0 split-offset length)
      (let [pivot (+ from split-offset)
            prev [[s from pivot] orig-acc] 
            next [[s pivot to] split-acc]] 
        (assoc phalange
               :left  (conj left prev)
               :right (conj right next)))
      phalange)))
  
(defn dactyl->split-phalange [dactyl]
  (let [acc (at-acc dactyl)]
    (-> dactyl
        :up
        (split-phalange acc))))

(defn split-dactyl [dactyl]
  (if (end-of-zipper? dactyl :left)
    dactyl
    (assoc dactyl :left nil
                  :up (-> dactyl dactyl->split-phalange))))

;; ## Combing

(defn comb [phalange]
  (let [acc-fn (:acc-fn (meta phalange))
        [[_ acc] :as rights] (:right phalange)]
    (->> rights
         (map first)
         (pair-reductions acc-fn acc)
         (assoc phalange :right))))

;; ## Modifying the buffer

(defn insert [dactyl string]
  (let [piece (string->piece string)
        acc (at-acc dactyl)]
    (-> dactyl
        split-dactyl
        :up
        (update :right (partial cons [piece acc]))
        comb
        phalange->dactyl)))

(defn cut [dactyl movement]
  ;; split both origin and end point, and make sure that both dactyls
  ;; have both splits in their piece lists by moving *back* to origin
  ;; this allows us to take-while identical? instead of having to do
  ;; any more complicated calculation.
  (let [d1 (-> dactyl split-dactyl movement split-dactyl)
        d2 (-> d1 (go-to (at-pos dactyl)))
        [pl pr] (map :up (sort-by at-pos [d1 d2]))
        ;;; TODO split these 2 functions here
        acc (at-acc pl)
        [[target _] & rights] (:right pr)


        ;; TODO perhaps make target the whole thing and swap the map and the take-while?
        cut-pieces (take-while (complement (partial identical? target))
                               (map first (:right pl)))
        d' (-> pl
               (assoc :right (cons [target acc] rights))
               comb
               phalange->dactyl)]
    [d' cut-pieces])) 

(def delete (comp first cut))

(comment
  (def dactyl (make-dactyl ["The cat\n" "Sat on\n" "The mat\n"]))
  (map at-char (take 26 (stream dactyl :right))))
