(ns pterodactyl.phalange-test
  (:require [clojure.test :refer :all]
            [pterodactyl.phalange :as ph])
  (:import pterodactyl.phalange.Piece
           pterodactyl.phalange.Table))

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
    
  

