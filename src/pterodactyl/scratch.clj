(ns pterodactyl.scratch
  (:require [automat.core :as a]
            [automat.viz :refer [view]])
  (:gen-class))

(defn pair-reductions [acc-fn init xs]
  (map list
       (concat xs [:end])
       (reductions acc-fn init xs)))

(defn make-zip-reducer [acc-fn init xs]
  (let [rights (pair-reductions acc-fn init xs)]
    ^{:acc-fn acc-fn}
    {:right rights
     :left '()}))

(def reversed {:left :right
               :right :left}) 

(def end-of-buffer? {:left empty?
                     :right (comp (partial = :end) first)})

(defn go [dir z]
  (let [[x & xs] (dir z)
        rev (reversed dir)]
    (if ((end-of-buffer? dir) x)
        z
        (assoc z dir xs
                 rev (conj (rev z) x)))))

(def go-left  (partial go :left))
(def go-right (partial go :right))

(defn modify [z n] 
  (let [{:keys [:right]} z
        acc-fn (:acc-fn (meta z))
        [[_ acc] & rights] right
        xs (cons n (map first (butlast rights)))
        rights (pair-reductions acc-fn acc xs)]
    (assoc z :right rights)))

(defn pos+ [m] (update m :pos inc))
(defn row+ [m] (update m :row inc))
(defn col0  [m] (assoc m :col 0))
(def crlf (comp row+ col0))
(defn col+ [m] (update m :col inc))

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

;;(def word (apply a/or (map list (concat (char-range \A \Z) (char-range \a \z)))))

(def word-count-with-signal
  (a/compile 
    (a/*
      (a/or 
        [(a/+ false)]
        [[(a/+ true) false] (a/$ :word+)]))
    {:signal (fn [c] (boolean (re-matches #"\w" (str c))))
     :reducers {:word+ word+}}))

(:value (reduce #(a/advance word-count-with-signal %1 %2) 0 "baa aa aaaaaa.")) ;=> 3

(defn word+ [v _] (inc v))
(def word [\a])
(def !word [(a/not \a)])
(def !word [\space])
(def word-count-with-not
  (a/compile 
    (a/*
      (a/or 
        [word !word (a/$ :word+)]
        [a/any]))
    {:reducers {:word+ word+}}))

(:value (reduce #(a/advance word-count-with-not    %1 %2) 0 "aaa aa aaaaaa ")) ;=> 6

(view word-count-with-not)

(defn acc-word-count [m c] (update m :word-count #(a/advance word-count-with-signal % c))

(defn set-eol [m] (assoc m :eol? true))
(defn unset-eol [m] (dissoc m :eol?))

(defn acc-basic [m c]
  (let [maybe-crlf (if (:eol? m) crlf identity)
        handle-eol (if (= \newline c) set-eol unset-eol)]
    (-> m
        pos+
        maybe-crlf
        handle-eol)))

(defn acc-fn [m c]
  (-> m
      (acc-basic c)
      (acc-word-count c)))

(def acc-init {:pos 0, :row 0, :col 0, :word-count 0})
;;(def acc-init {:word-count 0})

(->> (make-zip-reducer acc-fn acc-init (seq "a aaa\n\n\n\n\n\naaa aa\naaaaaaaaa aa"))
    :right)
    ;(map (juxt first (comp :value :word-count last))))
 
