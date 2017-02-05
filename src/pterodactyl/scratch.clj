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

(defn length [[_ from to]]
  (- to from))

(defn string->piece [s]
  [s 0 (count s)])

(def reversed {:left :right
               :right :left}) 

;; left and right are not mirror-images... ends look like:   [1 2 3 :end]
;; so:
;;    at left, have :left nil
;;    at right, have :right ($end-item) e.g. 1 item, which will be either
;;     :end (for phalange zipper)
;;     last-char (for dactyl zipper)
(defn end-of-zipper? [z dir]
  (let [eoz? {:left empty?
              :right (comp (partial = 1) count)}]
    ((dir eoz?) (dir z))))

(defn traverse [z dir]
  (if (end-of-zipper? z dir)
      nil
      (let [[x & xs] (dir z)
            rev (reversed dir)]
        (assoc z dir xs
                 rev (conj (rev z) x)))))

(defn end [dactyl]
  (let [[x xs] ((juxt last butlast) (:right dactyl))]
    (assoc dactyl :right (list x)
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

(defn acc-char [m c]
    (-> m
        pos++
        (col-or-row++ c)))

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

(defn phalange->dactyl [phalange dir]
  (let [[piece init] (first (:right phalange)) 
        xs (piece->seq piece)
        acc-fn-piece (:acc-fn-piece phalange)
        traverse-into (dir {:left end, :right identity})
        dactyl (make-zipper acc-fn-piece
                            init
                            xs
                            {:up phalange})] 
    (-> dactyl
        traverse-into)))

(defn at-char [{[[char _]] :right}]
  char)

(defn at-acc [{[[_ acc]] :right}]
  acc)

(def at-pos (comp :pos at-acc))
(def at-col (comp :col at-acc))

(defn debug [ting string] (println string) ting)
(defn debug-dactyl [dactyl]
  (debug dactyl (str "@ " (at-acc dactyl) "{" (at-char dactyl) "}"))) 

(defn make-dactyl [strings]
  (-> strings
      strings->phalange
      (phalange->dactyl :right)))

(defn go [dactyl dir]
  (or
    (some-> dactyl
            (traverse dir))
    (some-> dactyl :up
            (traverse dir)
            (phalange->dactyl dir))))

;; todo replace with unrolled version
(defn partial> [f & end-args]
  (fn [& start-args] (apply f (concat start-args end-args)))) 

(defn stream [dactyl dir]
  (take-while (complement nil?)
              (iterate (partial> go dir) dactyl))) 

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

;; TODO go via phalange
(defn go-start-of-buffer [dactyl]
  (-> dactyl
      (traverse-find :left (comp zero? :pos at-acc))))

(defn all-pos [dactyl]
  (-> dactyl
      go-start-of-buffer
      (#(map at-pos (stream % :right)))))

(defn all-text [dactyl]
  (-> dactyl
      go-start-of-buffer
      :up
      (#(apply str (map (comp string first) (butlast (:right %)))))))

(defn go-start-of-line [dactyl]
  (-> dactyl
      (traverse-find :left (comp zero? at-col))))

(defn go-end-of-line [dactyl]
  (-> dactyl
      (find-char :right \newline)))

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

(defn split-phalange [phalange split-acc]
  (let [left (:left phalange)
        [[[s from to] acc] & right] (:right phalange)
        split-offset (- (:pos split-acc) (at-pos phalange))]
    (if (< 0 split-offset (- to from))
      (let [prev [[s from (+ from split-offset)] acc] 
            next [[s (+ from split-offset) to] split-acc]] 
        (assoc phalange
               :right (conj right next)
               :left  (conj left prev)))
      phalange)))
  
(defn split-dactyl [dactyl]
  (if (end-of-zipper? dactyl :left)
    dactyl
    (assoc dactyl :left nil
                  :up (split-phalange (:up dactyl) (at-acc dactyl)))))

(comment
  (def dactyl (make-dactyl ["The cat\n" "Sat on\n" "The mat\n"]))
  (map at-char (take 26 (stream dactyl :right))))
