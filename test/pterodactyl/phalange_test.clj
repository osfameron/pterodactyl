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
    
(deftest test-dactyl
  (let [strings ["Hello" " " "World"]
        table (ph/make-table strings)
        dactyl (ph/make-dactyl table)]
    (testing "make-dactyl function"
      (isa? dactyl Dactyl)
      (is (= 0 (:acc-pos dactyl)))
      (is (= 0 (:curr-pos dactyl)))
      (is (= '() (:back dactyl)))
      (is (= "Hello World" (ph/text-after dactyl 100))))
    (testing "current/dactyl pos/text functions functions"
      (is (= (ph/make-piece "Hello") (ph/curr dactyl)))
      (is (= "Hello" (ph/curr-text dactyl)))
      (is (= "Hello" (ph/curr-text-post dactyl)))
      (is (= 5 (ph/curr-pos-post dactyl)))
      (is (= "" (ph/curr-text-pre dactyl)))
      (is (= 0 (ph/dactyl-pos dactyl))))
    (testing "traverse-forward"
      (let [d2 (ph/traverse-forward dactyl)]
        (is (= " World" (ph/text-after d2 100)))
        (is (= " " (ph/curr-text d2)))
        (is (= " " (ph/curr-text-post d2)))
        (is (= 1 (ph/curr-pos-post d2)))
        (is (= "" (ph/curr-text-pre d2)))
        (is (= 5 (ph/dactyl-pos d2)))))
    (testing "traverse-forward then traverse-back"
      (let [d3 (-> dactyl ph/traverse-forward ph/traverse-back)]
        (is (= (assoc dactyl :curr-pos 5) d3))))
    (testing "traverse-right"
      (let [string (clojure.string/join strings)
            length (count string)
            tr (fn [d jumps] (reduce ph/traverse-right d jumps))
            test-at (fn [d pos] 
                      (is (= pos (ph/dactyl-pos d)))
                      (is (= (subs string pos) (ph/text-after d 100))))]
        (testing "jump in one go"
          (doseq [i (range length)]
            (let [d2 (ph/traverse-right dactyl i)]
              (test-at d2 i))))
        (testing "jump by 1s"
          (doseq [i (range length)]
            (let [d3 (tr dactyl (repeat i 1))]
              (test-at d3 i))))
        (testing "jump from pos 4"
          (doseq [i (range 4 length)]
            (let [d4 (tr dactyl [4 (- i 4)])]
              (test-at d4 i))))
        (testing "jump from pos 5"
          (doseq [i (range 4 length)]
            (let [d4 (tr dactyl [4 (- i 4)])]
              (test-at d4 i))))))
    (testing "Assertion errors"
      (is (thrown? AssertionError (ph/make-dactyl "Single")))
      (is (thrown? AssertionError (ph/make-dactyl ["Hello" " " "World"]))))))

