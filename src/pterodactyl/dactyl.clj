(ns pterodactyl.dactyl
  (:require [net.cgrand.seqexp :as se]) 
  (:gen-class))

;; ## Pterodactyl: a programmer's text editor for winged dinosaurs

;; ### Making paired zippers
;;
;; The primary data-structure for Pterodactyl's text buffers is a
;; zipper over a sequence of *pairs* of
;;
;;   - a text object
;;   - the accumulated position of the text object
;; 
;; The accumulated positions are created as the reduction of an
;; "accumulator function".  For example, if the only piece of information
;; we cared about was the integer position within the buffer, we could
;; call: `(pair-reductions inc 0 (seq "abc")`
;; to get a sequence of:
;;
;;   `[[\a 0], [\b 1], [\c 2]]`
;;
;; (e.g. pairs of `[thing acc]`.  See "The Accumulator" below for details
;; of the actual accumulator functions we'll use.)
;;
;; As we then zip over this structure, we always know both what the character
;; at our cursor is, *and* what position we're at.

(defn pair-reductions 
  "Pair the supplied sequence with the reductions of the accumulator function
  and an initial value."
  [acc-fn init xs]
  (map list
       xs
       (reductions acc-fn init xs)))

(defn make-zipper
  "Makes a paired zipper. This will be a map that looks like
  `{:left (...) :right (...) ...}`

  Accepts a `base` argument which may contain additional keys.
  Additionally stores the `acc-fn` in metadata."
  [acc-fn init xs & [base]]
  (let [rights (pair-reductions acc-fn init xs)]
    (with-meta 
      (assoc base :right rights
                  :left nil)
      {:acc-fn acc-fn})))

(defn at-value
  "Return the value of the current position of the zipper"
  [{[[value _]] :right}]
  value)

(defn at-acc
  "Return the value of the current position of the zipper"
  [{[[_ acc]] :right}]
  acc)

(def reversed {:left :right
               :right :left}) 

;; left and right are not mirror-images... ends look like:   [1 2 3 :end]
;; so:
;;
;;   - at left, have :left nil
;;   - at right, have :right ($end-item)
;;     e.g. 1 item, which will be either)
;;        - :end (for phalange zipper)
;;        - or last-char (for dactyl zipper)
(defn end-of-zipper?
  "Boolean value if the zipper is at the end (by supplied `dir`)
  of its sequence."
  [z dir]
  (let [eoz? {:left empty?
              :right (comp empty? rest)}]
    ((dir eoz?) (dir z))))

(defn traverse
  "Generic zipper function to traverse to :left or :right by one step"
  [z dir]
  (if (end-of-zipper? z dir)
      nil
      (let [[x & xs] (dir z)
            rev (reversed dir)]
        (assoc z dir xs
                 rev (conj (rev z) x)))))

(defn end
  "Move the zipper to the end"
  [z]
  (let [[x xs] ((juxt last butlast) (:right z))]
    (assoc z :right (list x)
             :left (reverse xs))))

;; ## The Accumulator

;; (Plugins may add additional keys in future.)
(def acc-init
  "Our accumulator will (at simplest) have the `:pos` (offset of characters
  from the beginning of the buffer), `:row` (the line number) and `:col`
  (the column number), all zero-based."
  {:pos 0, :row 0, :col 0})

;; Helper routines to return pos/col/row of accumulator of focus of zipper
(def at-pos (comp :pos at-acc))
(def at-col (comp :col at-acc))
(def at-row (comp :row at-acc))

;; Accumulator functions take the next character in our text stream and return
;; an updated accumulator map.

;; ### Combinators to update position within buffer

;; First, we define some simple combinators to update the map.
(defn pos++ [m] (update m :pos inc))
(defn row++ [m] (update m :row inc))
(defn col++ [m] (update m :col inc))
(defn col0 [m] (assoc m :col 0))
(def crlf (comp row++ col0))

(defn col-or-row++
  "Increment col/row as appropriate, depending on whether the next
  char is a newline or not."
  [m c]
  (if (= \newline c)
    (crlf m)
    (col++ m)))

(defn acc-char
  "This is the base accumulator function.  It always increments the `:pos`
  offset, and handles row/col offsets."
  [m c]
  (-> m
      pos++
      (col-or-row++ c)))

;; ## The Piece
;;
;; Though we've so far only considered individual characters, the backbone
;; zipper in Pterodactyl is a zipper over "Pieces".  A piece is made up of
;; a string and the offsets into it.  This means that edits to a piece may
;; be done efficiently by e.g. moving the offsets.
;;
;; See [Wikipedia on Piece Tables](https://en.wikipedia.org/wiki/Piece_table)
;; for more details.

;; e.g.
;;
;; - `["Hello" 0 5]` => "Hello"
;; - `["Hello" 1 4]` => "ell"
(defn string
  "Get the string output of a piece, by applying its start and end offsets."
  [piece]
  (apply subs piece))

(defn length
  "Get the length of a piece"
  [[_ from to]]
  (- to from))

(defn string->piece
  "Get a piece from a string (with offsets to show the entire string.)"
  [s]
  [s 0 (count s)])

;; ### Accumulators over Pieces
;;
;; We already saw that accumulators work at the level of a single character. We
;; can easily accumulate over pieces (which are basically strings of
;; characters) by simply reduce'ing the accumulator over the sequence of
;; characters in the piece.

(defn piece->seq
  "Get the sequence of characters in a piece.
  If the piece is `:end` (a special token, representing the end of the buffer) will
  return a sequence of `[:end]`"
  [piece]
  (if (= :end piece)
      [piece]
      (seq (string piece))))

(defn make-acc-piece
  "Take an accumulator over chars and return an accumulator over pieces,
  by reducing the char accumulator over the sequence of chars."
  [acc-fn-char]
  (fn [m piece] (reduce acc-fn-char m (piece->seq piece))))

;; ## The Phalange
;;
;; The zipper over pieces is called the "Phalange".  (This is the word for the
;; bones of the finger - a finger is a type of pointer, as is a zipper, and the
;; zipper over the pieces is the underlying "bones" of the data-structure.)

(defn strings->phalange 
  "Given a list of strings, return a zipper over those pieces.
  May be passed an acc-char accumulator function (if omitted
  will use the `acc-char` function defined above.)"
  ([strings]
   (strings->phalange strings acc-char))

  ([strings acc-fn-char]
   (let [pieces (mapv string->piece strings)
         pieces (conj pieces :end)
         acc-fn (make-acc-piece acc-fn-char)]
     (make-zipper acc-fn
                  acc-init
                  pieces
                  {:acc-fn-char acc-fn-char}))))

;; ## The Dactyl
;;
;; The zipper over characters within a piece is called the "Dactyl".  (This
;; means finger, so it's the specific pointing abstraction that fleshes out
;; the bones of the Phalange.)

(defn phalange->dactyl
  "Returns a dactyl pointing at the first element of its
  parent phalange."
  [phalange]
  (let [[piece init] (first (:right phalange)) 
        xs (piece->seq piece)
        acc-fn-char (:acc-fn-char phalange)]
      (make-zipper acc-fn-char
                  init
                  xs
                  {:up phalange}))) 

;; TODO rename strings->dactyl, and take acc-fn-char
(defn make-dactyl
  [strings]
  (-> strings
      strings->phalange
      phalange->dactyl))

;; NB: not sure about naming of this function, or its parameters.
;; (e.g. should it be `(go-side (rev dir))`?)
(defn traverse-into
  "If moving :right into a dactyl, we want to start at the beginning,
  but if moving :left, we need to start at the end."
  [dactyl dir]
  (let [f (dir {:right identity, :left end})]
    (-> dactyl f)))

;; ### Debugging functions

(defn debug [ting string] (println string) ting)

(defn debug-dactyl [dactyl]
  (debug dactyl (str "@ " (at-acc dactyl) "{" (at-value dactyl) "}"))) 

;; ### Traversals
;; 
;; Traversal functions all take a dactyl and optionally some parameters and
;; return a new dactyl.  This MUST have no side-effects (e.g. recreating the
;; unzip'd piece table from the original dactyl and the post-traversal should
;; result in an identical datastructure.)

;; First, attempts to do a simple traverse within the dactyl.
;; If it's at the end of the dactyl, attempts to go via the
;; phalange instead.
(defn go 
  "Move one step in the appropriate direction.
  Returns `nil` if at the end of the buffer."
  [dactyl dir]
  (or
    (some-> dactyl
            (traverse dir))
    (some-> dactyl :up
            (traverse dir)
            phalange->dactyl
            (traverse-into dir))))

;; Utility function. TODO replace with unrolled version
(defn partial> [f & end-args]
  (fn [& start-args] (apply f (concat start-args end-args)))) 

(defn stream
  "A stream of dactyls heading in the appropriate direction."
  [dactyl dir]
  (->> dactyl
       (iterate (partial> go dir))
       (take-while (complement nil?))))

(defn find-in-stream
  "Use a seqexp search on the stream.
  We search for the complement of the matcher (e.g. cases where the match
  function *doesn't* match), up to the limit of the repeater function 
  (either `se/*` or `se/repeat` for unbounded/bounded search respectively).
  Returns the next value after this repetition, which will either be:

  - a match
  - OR the next item after the repeat limit.
  - OR nil (if the stream was exhausted before the limit)"
  [stream matcher repeater]
  (->> stream
       (se/exec
         (repeater (complement matcher)))
       :rest
       first))

(defn traverse-find
  "Low-level traversal taking a direction, a matcher and an optional numeric
  traversal limit.
  Returns the dactyl unchanged if no match was found"
  [dactyl dir matcher & [limit]]
  (let [ds (stream dactyl dir) 
        repeater (if limit (partial se/repeat 0 limit) se/*)] 
    (or (find-in-stream ds matcher repeater)
        dactyl)))

(defn match-char [char dactyl]
  (= char (at-value dactyl))) 

(defn find-char
  "Find a character" 
  [dactyl dir c & [limit]]
  (let [matcher (partial match-char c)]
    (traverse-find dactyl dir matcher limit)))

(defn go-extreme-of-buffer
  "Go to the start (`:left`) or end (`:right`) of the buffer"
  [dactyl dir]
  (-> dactyl
      :up
      (stream dir)
      last
      phalange->dactyl))

(def go-start-of-buffer (partial> go-extreme-of-buffer :left))

(defn go-to
  "Go to a numeric pos offset in the buffer"
  [dactyl pos]
  (let [delta (- pos (at-pos dactyl))]
    (cond
      (zero? delta) dactyl
      (pos? delta) (-> dactyl (stream :right) (nth delta))
      (neg? delta) (-> dactyl (stream :left) (nth (- delta))))))

(defn go-start-of-line
  "Go to the start of the line (e.g. the first position to the `:left`
  where the `:col` is 0)"
  [dactyl]
  (-> dactyl
      (traverse-find :left (comp zero? at-col))))

(defn go-end-of-line
  "Go to the end of the line (e.g. the first position to the `:right`
  which is a newline character)"
  [dactyl]
  (-> dactyl
      (find-char :right \newline)))

;; TODO test
(defn go-to-row
  "Go to a specified row"
  [dactyl row]
  (let [curr-row (at-row dactyl)
        match-row (comp (partial = row) at-row)]
    (if (= row curr-row)
      (-> dactyl go-start-of-line)
      (let [match-row (comp (partial = row) at-row)
            dir (if (> row curr-row) :right :left)]
        (-> dactyl
            (traverse-find dir match-row)))))) 

(defn go-up
  "Go up one row, attempting to stay in the same column"
  [dactyl]
  (let [col (at-col dactyl)
        to-col (comp (partial >= col) at-col)]
    (-> dactyl
        go-start-of-line
        (go :left)
        (traverse-find :left to-col)))) 

(defn go-down
  "Go up one row, attempting to stay in the same column"
  [dactyl]
  (let [col (at-col dactyl)]
    (-> dactyl
        go-end-of-line
        (go :right)
        (find-char :right \newline col))))

;; ### Whole buffer information
;;
;; (for debugging)

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

;; ## Splitting
;;
;; If we're operating between two points, it's most convenient if
;; we can work at the level of whole pieces in the phalange.  So rather than
;; have to complicate our logic by worrying about offsets everywhere, we
;; have a small number of functions that "split" a phalange into two separate
;; pieces, or split a dactyl (i.e. split its parent phalange, putting the
;; dactyl now at the beginning of a piece.)

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

(defn split-dactyl
  "split-dactyl splits the dactyl, ensuring that it's now at the
  beginning of its parent phalange."
  [dactyl]
  (if (end-of-zipper? dactyl :left)
    dactyl
    (assoc dactyl :left nil
                  :up (-> dactyl dactyl->split-phalange))))

;; NB: we move *back* to d2, rather than using original dactyl,
;; to make sure both pointers are aware of each other's split.
(defn split-move
  "Applies the movement function, splitting the two dactyls at origin
  and end point and returning a vector of their *phalanges* sorted in
  position order."
  [dactyl movement]
  (let [d1 (-> dactyl split-dactyl movement split-dactyl)
        d2 (-> d1 (go-to (at-pos dactyl)))]
    (->> [d1 d2]
         (sort-by at-pos)
         (map :up))))

;; ## Combing

(defn comb 
  "Comb a phalange - i.e. recalculate all the accumulators to its
  right."
  [phalange]
  (let [acc-fn (:acc-fn (meta phalange))
        [[_ acc] :as rights] (:right phalange)]
    (->> rights
         (map first)
         (pair-reductions acc-fn acc)
         (assoc phalange :right))))

;; ## Modifying the buffer

(defn insert-pieces 
  "Insert the seq of pieces at current point."
  [dactyl pieces]
  (let [acc (at-acc dactyl)
        rights (zipmap pieces (repeat acc))]
        ; we only actually need acc on the first piece, as the remainder
        ; will be comb'd out.
    (-> dactyl
        split-dactyl
        :up
        (update :right (partial concat rights))
        comb
        phalange->dactyl)))

(defn insert
  "Insert the string at current point."
  [dactyl string]
  (-> dactyl
      (insert-pieces [(string->piece string)])))

;; TODO test
(defn copy-pieces 
  "Copies the pieces between the two phalanges"
  [[pl pr]]
  (let [target (first (:right pr))]
    (map first 
         (take-while (complement (partial identical? target))
                     (:right pl)))))

(defn delete-between
  "Deletes the span between the two phalanges"
  [[pl pr]]
  (let [acc (at-acc pl)
        [[piece _] & rights] (:right pr)]
    (-> pl
        (assoc :right (cons [piece acc] rights))
        comb)))

(defn delete
  "Delete the span created by the movement."
  [dactyl movement]
  (-> dactyl
      (split-move movement)
      delete-between
      phalange->dactyl))
