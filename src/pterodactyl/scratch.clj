(ns pterodactyl.scratch
  (:gen-class))

(defn pair-reductions [acc-fn init xs]
  (map list
       (concat xs [:end])
       (reductions acc-fn init xs)))

(defn make-zip-reducer [acc-fn xs]
  (let [init (acc-fn)
        rights (pair-reductions acc-fn init xs)]
    ^{:acc-fn acc-fn}
    {:right rights
     :left '()}))

(def reversed {:left :right
               :right :left}) 
(def eol? {:left empty?
           :right (comp (partial = :end) first)})

(defn go [dir z]
  (let [[x & xs] (dir z)
        rev (reversed dir)]
    (if ((eol? dir) x)
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

(-> (make-zip-reducer + '(1 2 3 4 5 6))
    go-right
    go-right
    (modify 10)
    go-right
    (modify 10)
    go-left
    go-right
    go-right
    go-left
    go-left
    go-left
    go-left
    go-left
    go-right)
