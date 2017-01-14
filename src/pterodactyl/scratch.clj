(ns pterodactyl.scratch
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
(defn set-eol [m] (assoc m :eol? true))
(defn unset-eol [m] (dissoc m :eol?))

(defn acc-piece [m c]
  (let [crlf-or-advance (if (:eol? m) crlf col+)
        handle-eol (if (= \newline c) set-eol unset-eol)]
    (-> m
        pos+
        crlf-or-advance
        handle-eol)))

(def acc-init {:pos 0, :row 0, :col 0})

(defn string->piece [s]
  [s 0 (count s)])

(defn string [piece]
  (apply subs piece))

(defn acc-table [m p]
  (reduce acc-piece m (seq (string p))))

(->> (make-zip-reducer acc-piece acc-init (seq "Hello"))
    :right)

(->> (make-zip-reducer acc-table acc-init (map string->piece ["In " "Xanada\n" "did Kublai Khan"]))
     :right)
 
