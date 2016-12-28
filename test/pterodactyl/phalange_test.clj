(ns pterodactyl.phalange-test
  (:require [clojure.test :refer :all]
            [pterodactyl.phalange :as ph])
  (:import pterodactyl.phalange.Piece
           pterodactyl.phalange.Table
           pterodactyl.phalange.Dactyl))

(deftest test-piece
  (let [piece (ph/make-piece "Hello")]
    (testing "make-piece function"
      (isa? piece Piece)
      (is (= "Hello" (:string piece)))
      (is (= 0 (:from piece)))
      (is (= 5 (:to piece))))
    (testing "Assertion errors"
      (is (thrown? AssertionError (ph/make-piece ["Hello"]))))
    (testing "piece-length (on initial create)"
      (is (= 5 (ph/piece-length piece))
        (is (thrown? AssertionError (ph/piece-length "X")))))
    (testing "piece-string"
      (is (= "Hello" (ph/piece-string piece))))))

(deftest split-piece
  (let [string "Hello World"
        piece (ph/make-piece string)
        length (count string)]
    (testing "split-piece function & piece-string"
      (is (= [piece] (ph/split-piece piece 0)))
      (doseq [at (range 2 length)]
        (let [[piece1 piece2] (ph/split-piece piece at)]
          (is at (ph/piece-length piece1))
          (is (- length at) (ph/piece-length piece2))
          (is (str (ph/piece-string piece1) (ph/piece-string piece2)))))) 
    (testing "Assertion errors"
      (is (thrown? AssertionError (ph/split-piece "Hello" 1)))
      (is (thrown? AssertionError (ph/split-piece piece -1)))
      (is (thrown? AssertionError (ph/split-piece piece length))))))
    
(deftest test-table
  (let [table (ph/make-table ["Hello" " " "World"])]
    (testing "make-table function"
      (isa? table Table)
      (is (= "Hello World" (ph/show-table table))))
    (testing "Assertion errors"
      (is (thrown? AssertionError (ph/make-table "Single")))
      (is (thrown? AssertionError (ph/make-table ["Hello" 1])))
      (is (thrown? AssertionError (ph/show-table "Hello"))))))

(defmacro test-dactyl-at [d pos#]
  "Macro instead of function, purely to give failure message with line-number
  of caller. This expects to be run within test-dactyl, as it requires the
  'string' binding"
    `(do
       (is (= ~pos# (ph/dactyl-pos ~d)))
       (is (= (subs ~'string ~pos#) (ph/text-after ~d 100)))))

(deftest test-dactyl
  (let [strings ["Hello" " " "World"]
        table (ph/make-table strings)
        string (apply str strings)
        length (count string)
        dactyl (ph/make-dactyl table)]
    (testing "make-dactyl function"
      (isa? dactyl Dactyl)
      (is (= 0 (:acc-pos dactyl)))
      (is (= 0 (:curr-pos dactyl)))
      (is (= '() (:back dactyl))))
    (testing "current/dactyl pos/text functions functions"
      (is (= (ph/make-piece "Hello") (ph/curr dactyl)))
      (is (= "Hello" (ph/curr-text dactyl)))
      (is (= "Hello" (ph/curr-text-post dactyl)))
      (is (= 5 (ph/curr-pos-post dactyl)))
      (is (= "" (ph/curr-text-pre dactyl)))
      (test-dactyl-at dactyl 0))
    (testing "traverse-forward"
      (let [d2 (ph/traverse-forward dactyl)]
        (is (= " " (ph/curr-text d2)))
        (is (= " " (ph/curr-text-post d2)))
        (is (= 1 (ph/curr-pos-post d2)))
        (is (= "" (ph/curr-text-pre d2)))
        (test-dactyl-at d2 5)))
    (testing "traverse-forward then traverse-back"
      (let [d3 (-> dactyl ph/traverse-forward ph/traverse-back)]
        (is (= (assoc dactyl :curr-pos 4) d3))
        (test-dactyl-at d3 4)
        (is (= "Hello" (ph/curr-text d3)))
        (is (= "o" (ph/curr-text-post d3)))))
    (testing "traverse-right"
      (let [tr (fn [d jumps] (reduce ph/traverse-right d jumps))]
        (testing "jump in one go"
          (doseq [i (range length)]
            (let [d4 (ph/traverse-right dactyl i)]
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
        (testing "bounce :right"
          (let [d8    (tr dactyl [(dec length)])
                d8'   (tr dactyl [length])
                d8''  (tr dactyl [(dec length) 1])
                d8''' (tr dactyl [100])]
              (is (= nil (:bounce d8))) ; sanity
              (is (= d8' (assoc d8 :bounce :right)))
              (is (= d8' d8'' d8'''))))))
    (testing "Traverse right and left"
      (let [ds-right (take length (iterate ph/nudge-right dactyl))
            dactyl-end (ph/traverse-right dactyl (dec length))
            ds-left  (take length (iterate ph/nudge-left dactyl-end))
            exp-dactyl-pos's    (range length)
            exp-curr-pos's      [0 1 2 3 4 0 0 1 2 3 4]
            exp-curr-pos-post's [5 4 3 2 1 1 5 4 3 2 1]
            exp-curr-text-post's ["Hello" "ello" "llo" "lo" "o"
                                  " "
                                  "World" "orld" "rld" "ld" "d"]]
          (testing "to the right..."
            (is (= exp-dactyl-pos's (map ph/dactyl-pos ds-right)))
            (is (= exp-curr-pos's (map :curr-pos ds-right)))
            (is (= exp-curr-pos-post's (map ph/curr-pos-post ds-right)))
            (is (= exp-curr-text-post's (map ph/curr-text-post ds-right))))
          (testing "to the left..."
            (is (= (reverse exp-dactyl-pos's) (map ph/dactyl-pos ds-left)))
            (is (= (reverse exp-curr-pos's) (map :curr-pos ds-left)))
            (is (= (reverse exp-curr-pos-post's) (map ph/curr-pos-post ds-left)))
            (is (= (reverse exp-curr-text-post's) (map ph/curr-text-post ds-left))))))

    (testing "Assertion errors"
      (is (thrown? AssertionError (ph/make-dactyl "Single")))
      (is (thrown? AssertionError (ph/make-dactyl ["Hello" " " "World"]))))))

