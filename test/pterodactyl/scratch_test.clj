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

;; helpers to do is testing within a -> pipeline

(defn is= [x y]
  (is (= x y))
  x)

(defn is? [x pred? y]
  (is (pred? x y))
  x)

(deftest test-acc-piece
  (-> acc-init
      (acc-piece \a)
      (is= {:pos 1 :row 0 :col 1})

      (acc-piece \b)
      (is= {:pos 2 :row 0 :col 2})

      (acc-piece \newline)
      (is= {:pos 3 :row 0 :col 3 :eol? true})

      (acc-piece \a)
      (is= {:pos 4 :row 1 :col 0})

      (acc-piece \b)
      (is= {:pos 5 :row 1 :col 1})

      (acc-piece \newline)
      (is= {:pos 6 :row 1 :col 2 :eol? true})

      (acc-piece \newline)
      (is= {:pos 7 :row 2 :col 0 :eol? true})

      (acc-piece \newline)
      (is= {:pos 8 :row 3 :col 0 :eol? true})

      (acc-piece \a)
      (is= {:pos 9 :row 4 :col 0})

      (acc-piece \b)
      (is= {:pos 10 :row 4 :col 1})))
