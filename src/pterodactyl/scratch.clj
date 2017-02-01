(ns pterodactyl.scratch
  (:require [net.cgrand.seqexp :as se]) 
  (:gen-class))

(defn pair-reductions [acc-fn init xs]
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

(defn string [piece]
  (apply subs piece))

(defn string->piece [s]
  [s 0 (count s)])

(def reversed {:left :right
               :right :left}) 

;; left and right are not duals... ends look like:   [1 2 3 :end]
;; so:
;;    at left, have :left nil
;;    at right, have :right ($end-item) e.g. 1 item, which will be either
;;     :end (for phalange zipper)
;;     last-char (for dactyl zipper)
(defn end-of-zipper? [z dir]
  (let [fs {:left (comp empty?)
            :right #(= 1 (count %))}]
    ((dir fs) (dir z))))

(defn traverse [z dir]
  (let [[x & xs] (dir z)
        rev (reversed dir)]
    (if (end-of-zipper? z dir)
        z
        (assoc z dir xs
                 rev (conj (rev z) x)))))

(defn end [dactyl]
  (let [right (:right dactyl)
        xs (butlast right)
        x (last right)]
    (assoc dactyl :right [x]
                  :left (reverse xs))))

; generic modify function (probably not useful in this form for us)
(defn modify [z n] 
  (let [{:keys [:right]} z
        acc-fn (:acc-fn (meta z))
        [[_ acc] & rights] right
        xs (cons n (map first (butlast rights)))
        rights (pair-reductions acc-fn acc xs)]
    (assoc z :right rights)))

; combinators to update position within buffer
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

(defn acc-piece [m c]
    (-> m
        pos++
        (col-or-row++ c)))

(defn piece->seq [piece]
  (if (= :end piece)
      []
      (seq (string piece))))

;; TODO: instead, make-phalange should be passed the acc-piece acculumator and
;; contruct the table accumulator based on it.
(defn acc-table [m p]
  (reduce acc-piece m (piece->seq p)))

(defn strings->phalange [strings]
  (let [pieces (mapv string->piece strings)
        pieces (conj pieces :end)]
    (make-zipper acc-table acc-init pieces)))

(def traverse-into-dactyl {:left end, :right identity})

(defn phalange->dactyl [phalange dir]
  (let [[piece init] (first (:right phalange)) 
        xs (piece->seq piece)
        dactyl (make-zipper acc-piece init xs {:up phalange})] 
    ((traverse-into-dactyl dir) dactyl)))

(defn debug [ting string] (println (str ting " - " string) ting))

(defn make-dactyl [strings]
  (-> strings
      strings->phalange
      (phalange->dactyl :right)))

(defn go [dactyl dir]
  (if (end-of-zipper? dactyl dir)
    (let [up (:up dactyl)]
      (if (end-of-zipper? up dir)
        nil
        (-> up
            (traverse dir)
            (phalange->dactyl dir))))
    (let [next (traverse dactyl dir)]
        next)))

;; todo replace with unrolled version
(defn partial> [f & end-args]
  (fn [& start-args] (apply f (concat start-args end-args)))) 

(defn stream [dactyl dir]
  (take-while (complement nil?)
              (iterate (partial> go dir) dactyl))) 

(defn at-col [{[[_ {col :col}]] :right}]
  col)

(defn at-char [{[[char]] :right}]
  char)

(defn match-char [char dactyl]
  (= char (at-char dactyl))) 

(defn traverse-find [dactyl dir matcher]
  (let [ds (stream dactyl dir)] 
     (if-let [m (->> ds
                     (se/exec (se/* (complement matcher))) 
                     :rest
                     first)]
        m
        dactyl)))

(defn find-char [dactyl dir c]
  (let [matcher (partial match-char c)]
    (traverse-find dactyl dir matcher)))

(defn start-of-line? [dactyl]
  (zero? (at-col dactyl)))

(defn go-start-of-line [dactyl]
  (traverse-find dactyl :left start-of-line?))

(defn go-end-of-line [dactyl]
  (find-char dactyl :right \newline))

(comment
  (def dactyl (make-dactyl ["The cat\n" "Sat on\n" "The mat\n"]))
  (map at-char (take 26 (stream dactyl :right))))
