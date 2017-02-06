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

(deftest test-acc-char
  (is (= [[\a {:pos 0 :row 0 :col 0}]
          [\b {:pos 1 :row 0 :col 1}]
          [\newline {:pos 2 :row 0 :col 2}]
          [\a {:pos 3 :row 1 :col 0}]
          [\b {:pos 4 :row 1 :col 1}]
          [\c {:pos 5 :row 1 :col 2}]
          [\newline {:pos 6 :row 1 :col 3}]
          [\newline {:pos 7 :row 2 :col 0}]
          [\a {:pos 8 :row 3 :col 0}]]
         (pair-reductions acc-char acc-init (seq "ab\nabc\n\na")))))

;; helpers to do is testing within a -> pipeline

(defn is= [x y]
  (is (= x y))
  x)

(defn is=char [dactyl c]
  (is (= c (at-char dactyl)))
  dactyl)

(defn is=pos [dactyl p]
  (is (= p (at-pos dactyl)))
  dactyl)

(defn is? [x pred?]
  (is (pred? x))
  x)

(def d2 (make-dactyl ["The cat\n" "Sat on\n" "The mat\n"]))

(deftest end-and-start-of-line
  (-> d2
    (is=char \T)
    (go-end-of-line)
    (is=char \newline)
    (go-start-of-line)
    (is=char \T)
    (go-end-of-line)
    (is=char \newline)
    (go :right)
    (go-end-of-line)
    (is=char \newline)
    (go-start-of-line)
    (is=char \S)
    (go :right)
    (is=char \a)
    (go-down)
    (is=char \h)
    (go-up)
    (is=char \a)
    (is? #(not (end-of-zipper? % :left))) 
    (split-dactyl)
    (is? #(end-of-zipper? % :left)) 
    (is=char \a)
    (go :left)
    (is? #(end-of-zipper? % :right)) 
    (is=char \S)
    (is? #(= "The cat\nSat on\nThe mat\n" (all-text %)))))

(deftest back-and-forth-forever
  (is (= d2
         (-> d2
             (is=pos 0)
             (stream :right)
             last
             (is=pos 23)
             (stream :left)
             last
             (is=pos 0))))
  (is (= (-> d2
             (is=pos 0)
             (stream :right)
             last
             (is=pos 23))
         (-> d2
             (is=pos 0)
             (stream :right)
             last
             (is=pos 23)
             (stream :left)
             last
             (is=pos 0)
             (stream :right)
             last
             (is=pos 23)))))

(deftest insertion
  (is (= "Yes The cat\nSat on\nThe mat\n"
         (-> d2
             (insert "Yes ")
             all-text)))
  (is (= "The cat did\nSat on\nThe mat\n"
         (-> d2
             go-end-of-line
             (insert " did")
             all-text)))
  (is (= "The cat\nDid Sat on\nThe mat\n"
         (-> d2
             go-down
             (insert "Did ")
             all-text))))

    ;(#(count (stream % :right)))))
    ;(#(debug % (all-pos %)))))
