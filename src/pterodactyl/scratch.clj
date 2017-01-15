(ns pterodactyl.scratch
  (:gen-class))

(defn pair-reductions [acc-fn init xs]
  (map list
       (concat xs [:end])
       (reductions acc-fn init xs)))

; zipper is of [[thing acc] [thing acc] ...]
(defn make-zipper [acc-fn init xs & [metadata]]
  (let [rights (pair-reductions acc-fn init xs)]
    (with-meta
      {:right rights
       :left '()}
      (assoc metadata :acc-fn acc-fn))))

(def reversed {:left :right
               :right :left}) 

;; left and right are not duals... ends look like:   [1 2 3 :end]
;; so:
;;    at left, have :left ()
;;    at right, have :right (_, :end) e.g. 2 items
(defn end-of-zipper? [z dir]
  (let [x (dir z)
        fs {:left (comp empty?)
            :right (comp (partial = :end) first second)}]
    ((dir fs) x)))

(defn traverse [z dir]
  (let [[x & xs] (dir z)
        rev (reversed dir)]
    (if (end-of-zipper? z dir)
        z
        (assoc z dir xs
                 rev (conj (rev z) x)))))

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
(defn col-or-row++ [m]
  (if (:eol? m)
    (crlf m)
    (col++ m)))
(defn update-eol-state [m c]
  (if (= \newline c)
    (assoc m :eol? true)
    (dissoc m :eol?)))

(defn string->piece [s]
  [s 0 (count s)])

(defn string [piece]
  (apply subs piece))

(def acc-init {:pos 0, :row 0, :col 0})

(defn acc-piece [m c]
    (-> m
        pos++
        col-or-row++
        (update-eol-state c)))

(defn acc-table [m p]
  (reduce acc-piece m (seq (string p))))

(defn make-phalange [strings]
  (let [pieces (map string->piece strings)]
    (make-zipper acc-table acc-init pieces)))

(defn end [dactyl]
  (let [right (:right dactyl)
        xs (drop-last 2 right)
        x (take-last 2 right)]
    (assoc dactyl :right x
                  :left (reverse xs))))

(defn phalange->dactyl [phalange & [dir]]
  (let [[piece init] (first (:right phalange)) 
        xs (if (= :end piece) [] (seq (string piece)))
        dactyl (make-zipper acc-piece init xs {:up phalange})] 
    (if (= :left dir)
      (end dactyl)
      dactyl)))

(defn make-dactyl [strings]
  (-> strings
      make-phalange
      phalange->dactyl))

(defn go [dactyl dir]
  (if (end-of-zipper? dactyl dir)
    (let [up (:up (meta dactyl))]
      (if (end-of-zipper? up dir)
        dactyl
        (-> up (traverse dir) (phalange->dactyl dir))))
    (let [next (traverse dactyl dir)]
        next)))

;; todo replace with unrolled version
(defn partial> [f & end-args]
  (fn [& start-args] (apply f (concat start-args end-args)))) 

(defn stream [dactyl dir]
  (iterate (partial> go dir) dactyl)) 

(defn at-char [dactyl]
  (let [[[char]] (:right dactyl)]
    char))

(def dactyl (make-dactyl ["In " "Xanadu\n" "did Kublai Khan"]))

(comment
  (identity dactyl)
  (def five (nth (stream dactyl :right) 8))
  (map at-char (take 30 (stream five :right)))
  (map at-char (take 12 (stream five :left))))


