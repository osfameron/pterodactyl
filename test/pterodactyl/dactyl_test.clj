(ns pterodactyl.dactyl-test
  (:require [clojure.test :refer :all]
            [pterodactyl.dactyl :as d]
            [lanterna.screen :as s])
  (:import pterodactyl.dactyl.StringPiece
           pterodactyl.dactyl.Piece
           pterodactyl.dactyl.Dactyl))

(deftest test-piece
  (let [piece (d/string->piece "Hello")]
    (testing "string->piece function"
      (isa? piece StringPiece)
      ;(is (satisfies? Piece piece))
      (is (= "Hello" (:string piece)))
      (is (= 0 (:from piece)))
      (is (= 5 (:to piece))))
    (testing "Assertion errors"
      (is (thrown? AssertionError (d/string->piece ["Hello"]))))
    (testing "piece-length (on initial create)"
      (is (= 5 (d/piece-length piece))))
    (testing "piece-string"
      (is (= "Hello" (d/piece-string piece))))
    (testing "END-OF-BUFFER"
      (is (= 1 (d/piece-length d/END-OF-BUFFER)))
      (is (= "" (d/piece-string d/END-OF-BUFFER)))
      (is (= "<END-OF-BUFFER>" (str d/END-OF-BUFFER))))))

(deftest split-piece
  (let [string "Hello World"
        piece (d/string->piece string)
        length (count string)]
    (testing "split-piece function & piece-string"
      (is (= [d/END-OF-BUFFER] (d/split-piece d/END-OF-BUFFER 0)))
      (is (= [piece] (d/split-piece piece 0)))
      (doseq [at (range 2 length)]
        (let [[piece1 piece2] (d/split-piece piece at)]
          (is at (d/piece-length piece1))
          (is (- length at) (d/piece-length piece2))
          (is (str (d/piece-string piece1) (d/piece-string piece2)))))))) 
    
(defmacro test-dactyl-at [d pos#]
  "Macro instead of function, purely to give failure message with line-number
  of caller. This expects to be run within test-dactyl, as it requires the
  'string' binding"
    `(do
       (is (= ~pos# (d/dactyl-pos ~d)))
       (is (= (subs ~'string ~pos#) (d/text-after ~d)))))

; vaguely related to as-> except it doesn't do threading,
; has a hard-coded anaphoric 'it' and simply returns the value
; at end
(defmacro and-test [expr & forms]
  `(let [~'it ~expr]
     ~@forms
     ~'it))

(deftest test-dactyl
  (let [strings ["Hello" " " "World"]
        string (apply str strings)
        length (count string)
        dactyl (d/strings->dactyl strings)]
    (testing "strings->dactyl function"
      (isa? dactyl Dactyl)
      (is (= 0 (:acc-pos dactyl)))
      (is (= 0 (:curr-pos dactyl)))
      (is (= '() (:back dactyl))))
    (testing "current/dactyl pos/text functions functions"
      (is (= (d/string->piece "Hello") (d/curr dactyl)))
      (is (= "Hello" (d/curr-text dactyl)))
      (is (= "Hello" (d/curr-text-post dactyl)))
      (is (= 5 (d/curr-pos-post dactyl)))
      (is (= "" (d/curr-text-pre dactyl)))
      (test-dactyl-at dactyl 0))
    (testing "traverse-next"
      (let [d2 (d/traverse-next dactyl)]
        (is (= " " (d/curr-text d2)))
        (is (= " " (d/curr-text-post d2)))
        (is (= 1 (d/curr-pos-post d2)))
        (is (= "" (d/curr-text-pre d2)))
        (test-dactyl-at d2 5)))
    (testing "traverse-next then traverse-pretraverse-prev"
      (let [d3 (-> dactyl d/traverse-next d/traverse-prev)]
        (is (= (assoc dactyl :curr-pos 4) d3))
        (test-dactyl-at d3 4)
        (is (= "Hello" (d/curr-text d3)))
        (is (= "o" (d/curr-text-post d3)))))
    (testing "traverse-forward"
      (let [tr (fn [d jumps] (reduce d/traverse-forward d jumps))]
        (testing "jump in one go"
          (doseq [i (range length)]
            (let [d4 (d/traverse-forward dactyl i)]
              (test-dactyl-at d4 i))))
        (testing "jump by 1s"
          (doseq [i (range length)]
            (let [d5 (tr dactyl (repeat i 1))]
              (test-dactyl-at d5 i))))
        (testing "jump from pos 4"
          (doseq [i (range 4 length)]
            (let [d6 (tr dactyl [4 (- i 4)])]
              (test-dactyl-at d6 i))))
        (testing "jump from pos 5"
          (doseq [i (range 5 length)]
            (let [d7 (tr dactyl [5 (- i 5)])]
              (test-dactyl-at d7 i))))
        (testing "goto"
          (let [jumps (shuffle (range length))
                test-goto (fn [d j]
                            (let [d' (d/goto d j)
                                  d'' (d/goto d' j)]
                              (is (= d' d''))
                              (test-dactyl-at d' j)
                              d'))]
            (reduce test-goto dactyl jumps)))
        (testing "bounce :right"
          (let [d8    (tr dactyl [length])
                d8'   (tr dactyl [(inc length)])
                d8''  (tr dactyl [length 1])
                d8''' (tr dactyl [100])]
              (is (= nil (:bounce d8))) ; sanity
              (is (= (assoc d8 :bounce :right) d8'))
              (is (= d8' d8'' d8'''))))))
    (testing "Traverse right and left"
      (let [ds-right (take length (iterate d/traverse-forward dactyl))
            dactyl-end (d/traverse-forward dactyl (dec length))
            ds-left  (take length (iterate d/traverse-backward dactyl-end))
            exp-dactyl-pos's    (range length)
            exp-curr-pos's      [0 1 2 3 4 0 0 1 2 3 4]
            exp-curr-pos-post's [5 4 3 2 1 1 5 4 3 2 1]
            exp-curr-text-post's ["Hello" "ello" "llo" "lo" "o"
                                  " "
                                  "World" "orld" "rld" "ld" "d"]]
          (testing "to the right..."
            (is (= exp-dactyl-pos's (map d/dactyl-pos ds-right)))
            (is (= exp-curr-pos's (map :curr-pos ds-right)))
            (is (= exp-curr-pos-post's (map d/curr-pos-post ds-right)))
            (is (= exp-curr-text-post's (map d/curr-text-post ds-right))))
          (testing "to the left..."
            (is (= (reverse exp-dactyl-pos's) (map d/dactyl-pos ds-left)))
            (is (= (reverse exp-curr-pos's) (map :curr-pos ds-left)))
            (is (= (reverse exp-curr-pos-post's) (map d/curr-pos-post ds-left)))
            (is (= (reverse exp-curr-text-post's) (map d/curr-text-post ds-left))))))
    (testing "Assertion errors"
      (is (thrown? AssertionError (d/strings->dactyl "Single"))))
    (testing "split-dactyl"
      (doseq [d (take length (iterate d/traverse-forward dactyl))]
        (let [d-split (d/split-dactyl d)]
          (if (zero? (:curr-pos d))
            (is (= d d-split))
            (do
              (is (= (d/dactyl-pos d) (d/dactyl-pos d-split)))
              (is (= 0 (:curr-pos d-split)))
              (is (= (+ (:acc-pos d) (:curr-pos d)) (:acc-pos d-split)))
              (is (= (rest (:pieces d)) (rest (:pieces d-split))))
              (let [prev (d/traverse-prev d-split)
                    left (d/traverse-backward d-split)]
                (is (= prev left))
                (is (= (:acc-pos d) (:acc-pos prev)))
                (is (= (dec (:curr-pos d)) (:curr-pos prev)))))))))
    (testing "delete-to"
      (is (= "Helld" (-> dactyl 
                         (d/traverse-forward 4)
                         (d/delete-to #(d/goto % 10))
                         (d/goto 0) (d/text-after))))
      (is (= "Hell World" (-> dactyl
                             (d/traverse-forward 5)
                             (d/delete-to d/traverse-backward)
                             (d/goto 0) (d/text-after))))
      (is (= "World" (-> dactyl
                         (d/traverse-forward 6)
                         (d/delete-to #(d/goto % 0))
                         (d/text-after))))
      (is (= "Hello" (-> dactyl
                         (d/traverse-forward 5)
                         (d/delete-to #(d/traverse-forward % 100))
                         (d/goto 0) (d/text-after)))))
    (testing "insert"
      (is (= "So, Hello World" (-> dactyl
                                   (d/insert "So, ")
                                   (d/goto 0) (d/text-after))))
      (is (= "Hello there World" (-> dactyl
                                     (d/traverse-forward 6)
                                     (d/insert "there ")
                                     (d/goto 0) (d/text-after)))))
    (testing "copy-range"
      (is (= "" (-> dactyl
                    (d/copy-range identity)
                    (d/all-text))))
      (is (= "o" (-> dactyl
                     (d/traverse-forward 4)
                     (d/copy-range d/traverse-forward)
                     (d/all-text))))
      (is (= "o Worl" (-> dactyl
                         (d/traverse-forward 4)
                         (d/copy-range #(d/goto % 10))
                         (d/all-text)))))
    (testing "right-till"
      (is (= "o World" (-> dactyl
                          (d/right-till "o")
                          (d/text-after))))
      (is (= "orld" (-> dactyl
                          (d/right-till "o")
                          (d/right-till "o")
                          (d/text-after))))
      (is (= "" (-> dactyl
                          (d/right-till "o")
                          (d/right-till "o")
                          (d/right-till "o")
                          (d/text-after)))))
    (testing "left-till"
      (is (= "orld" (-> dactyl
                          (d/traverse-forward 100)
                          (d/left-till "o")
                          (d/text-after))))
      (is (= "o World" (-> dactyl
                          (d/traverse-forward 100)
                          (d/left-till "o")
                          (d/left-till "o")
                          (d/text-after))))
      (is (= "Hello World" (-> dactyl
                             (d/traverse-forward 100)
                             (d/left-till "o")
                             (d/left-till "o")
                             (d/left-till "o")
                             (d/text-after)))))
    (testing "rows and columns"
      (let [dactyl 
              (-> ["APRIL is the cruellest month, breeding\n"
                   "Lilacs out of the dead land, mixing\n"
                   "Memory and desire, stirring\n"
                   "Dull roots with spring rain.\n"
                   "Winter kept us warm, covering\n"
                   "Earth in forgetful snow, feeding\n"
                   "A little life with dried tubers.\n"]
                 (d/strings->dactyl))]
        (testing "col-pos"
          (-> dactyl
              (and-test
                (is (= 0 (d/col-pos it))))
              (d/traverse-forward 5)
              (and-test
                (is (= 5 (d/col-pos it))))
              (d/go-start-of-line)
              (and-test
                (is (= 0 (d/col-pos it))))
              (d/right-till "kept")
              (and-test
                (is (= 7 (d/col-pos it))))
              (d/go-end-of-line)
              (and-test
                (is (= 29 (d/col-pos it)))
                (is (= "\n" (d/text-after it 1))))))))))

; some examples for interacting with a visual editor
; no key-bindings yet.  Call like so, from repl:
;
; (require '[pterodactyl.dactyl :as d])
; (require '[pterodactyl.dactyl.test :as dt])
; (dt/init-screen)
; (dt/m d/traverse-forward 1)
;
(def d (atom (-> ["APRIL is the cruellest month, breeding\n"
                  "Lilacs out of the dead land, mixing\n"
                  "Memory and desire, stirring\n"
                  "Dull roots with spring rain.\n"
                  "Winter kept us warm, covering\n"
                  "Earth in forgetful snow, feeding\n"
                  "A little life with dried tubers."]
                (d/strings->dactyl))))

(def screen (atom nil))

(defn init-screen []
  (let [scr (s/get-screen)]
    (s/start scr)
    (reset! screen scr)))

(defn clear-screen []
  (let [[width height] (s/get-size @screen) 
        blank (apply str (repeat width " "))]
    (doseq [line (range height)] 
      (s/put-string @screen 0 line blank)))) 

(defn draw-screen [dactyl]
  (let [x (d/col-pos dactyl)
        y (d/row-pos dactyl)]
    (clear-screen)
    (doseq [[line text] (map list (range) (clojure.string/split-lines (d/all-text dactyl)))]
      (s/put-string @screen 0 line text)
      (s/move-cursor @screen x y))
    (s/redraw @screen)))

(defn m [move & args]
  (let [d' (swap! d #(apply move (concat [%] args)))]
    (println d')
    (draw-screen d')))

(comment
  (draw-screen @d)
  (doseq [
          line (range 0 7)
          col  (range 0 10)]
       (-> @d
           (d/traverse-down 7)
           (d/traverse-forward col)
           (d/traverse-up line)
           (and-test
              (println (str line col (d/text-after it 10)))))))
