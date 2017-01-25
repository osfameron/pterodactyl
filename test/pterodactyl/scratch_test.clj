(ns pterodactyl.scratch-test
  (:require [clojure.test :refer :all]
            [pterodactyl.scratch :refer :all]))

(deftest test-pair-reductions
  (is (= [[1 0] [2 1] [3 3] [4 6] [5 10]]
         (pair-reductions + 0 [1 2 3 4 5]))))

(defn acc-test [m _] (pos++ m))
(def zipper (make-zipper acc-test {:pos 0} (seq "Hello")))

(deftest test-make-zipper
  (is (= {:right
          '((\H {:pos 0})
            (\e {:pos 1})
            (\l {:pos 2})
            (\l {:pos 3})
            (\o {:pos 4}))
           :left nil}
         zipper))
  (is (= acc-test (-> zipper meta :acc-fn)))) 

(deftest test-string
  (is (= "Hello" (string ["Hello" 0 5])))
  (is (= "ll" (string ["Hello" 2 4]))))

(deftest test-string->piece
  (is (= ["Hello" 0 5] (string->piece "Hello"))))

(deftest test-reversed
  (is (= :left (reversed :right)))
  (is (= :right (reversed :left))))

(deftest test-ends
  (testing "end-of-zipper? :left"
    (is (end-of-zipper? zipper :left))
    (is (not (end-of-zipper? zipper :right))))
  (testing "end-of-zipper? :right"
    (let [zipper (end zipper)]
      (is (end-of-zipper? zipper :right))
      (is (not (end-of-zipper? zipper :left)))
      (is (= {:right
              '((\o {:pos 4}))
              :left
              '((\l {:pos 3})
                (\l {:pos 2})
                (\e {:pos 1})
                (\H {:pos 0}))}
           zipper)))))
      
(deftest test-traverse
  (let [pos1 (traverse zipper :right)
        pos0 (traverse pos1 :left)]
    (is (end-of-zipper? pos0 :left))
    (is (= zipper pos0))
    (is (= {:right
              '((\e {:pos 1})
                (\l {:pos 2})
                (\l {:pos 3})
                (\o {:pos 4}))
            :left
              '((\H {:pos 0}))}
           pos1))))
