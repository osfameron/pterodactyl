(ns pterodactyl.dactyl-test
  (:require [clojure.test :refer :all]
            [pterodactyl.dactyl :refer :all]))

(deftest test-pair-reductions
  (is (= [{:val 1, :acc 0}
          {:val 2, :acc 1}
          {:val 3, :acc 3}
          {:val 4, :acc 6}
          {:val 5, :acc 10}]
         (pair-reductions + 0 [1 2 3 4 5]))))

(defn acc-test [m _] (pos++ m))
(def zipper (make-zipper acc-test {:pos 0} (seq "Hello")))

(deftest test-make-zipper
  (is (= {:right
          '({:val \H, :acc  {:pos 0}}
            {:val \e, :acc  {:pos 1}}
            {:val \l, :acc  {:pos 2}}
            {:val \l, :acc  {:pos 3}}
            {:val \o, :acc  {:pos 4}})
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
              '({:val \o, :acc {:pos 4}})
              :left
              '({:val \l, :acc {:pos 3}}
                {:val \l, :acc {:pos 2}}
                {:val \e, :acc {:pos 1}}
                {:val \H, :acc {:pos 0}})}
           zipper)))))
      
(deftest test-traverse
  (let [pos1 (traverse zipper :right)
        pos0 (traverse pos1 :left)]
    (is (end-of-zipper? pos0 :left))
    (is (= zipper pos0))
    (is (= {:right
              '({:val \e, :acc {:pos 1}}
                {:val \l, :acc {:pos 2}}
                {:val \l, :acc {:pos 3}}
                {:val \o, :acc {:pos 4}})
            :left
              '({:val \H, :acc {:pos 0}})}
           pos1))))

(def NL \newline)
(deftest test-acc-char
  (is (= [{:val \a, :acc {:pos 0 :row 0 :col 0}}
          {:val \b, :acc {:pos 1 :row 0 :col 1}}
          {:val NL, :acc {:pos 2 :row 0 :col 2}}
          {:val \a, :acc {:pos 3 :row 1 :col 0}}
          {:val \b, :acc {:pos 4 :row 1 :col 1}}
          {:val \c, :acc {:pos 5 :row 1 :col 2}}
          {:val NL, :acc {:pos 6 :row 1 :col 3}}
          {:val NL, :acc {:pos 7 :row 2 :col 0}}
          {:val \a, :acc {:pos 8 :row 3 :col 0}}]
         (pair-reductions acc-char acc-init (seq "ab\nabc\n\na")))))

;; helpers to do is testing within a -> pipeline

(defn is= [x y]
  (is (= x y))
  x)

(defn is=char [dactyl c]
  (is (= c (at-value dactyl)))
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

(deftest test-delete
  (testing "Delete forward"
    (is (= "That\nSat on\nThe mat\n"
           (-> d2
               (find-char :right \e)
               (is=pos 2)
               (delete #(find-char % :right \a))
               (is=pos 2)
               all-text))))
  (testing "Delete backward"
    (is (= "That\nSat on\nThe mat\n"
           (-> d2
               (find-char :right \a)
               (is=pos 5)
               (delete #(find-char % :left \e))
               (is=pos 2)
               all-text)))))

(deftest test-go-to
  (is (= 10
         (-> d2
             (go-to 10)
             at-pos)))
  (is (= 15
         (-> d2
             (go-to 15)
             at-pos)))
  (is (= 5
         (-> d2
             (go-to 10)
             (go-to 5)
             at-pos))))

;; TODO e.g. test that accumulator is correctly comb'd on insertion
(comment
  (-> d2
     go-end-of-line
     ;(insert "\n\n")
     (#(doseq [p (stream % :right)] (println (at-acc p))))))
