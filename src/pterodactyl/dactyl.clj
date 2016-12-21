;;;; dactyl function sketch
; TODO handle docstring (& other parameters?)
(defmacro defmove [fname body]
  `(defn ~(symbol (name fname)) 
     [~(symbol "&dactyl")]
     eval ~body))

(defmacro => [& forms]
  `(map (partial apply ~(symbol "&dactyl")) '(~@forms)))

(defmove noop
  (=> (:right)
      (:left)))

; example call
(let [dactyl-function println]
  (noop dactyl-function))

(comment
  (defuser my-function [param]
    ; &dactyl and &count are passed in implicitly
    ; e.g. from repl (call my-function "hello")
    ; would actually call (my-function dactyl "hello")
    ; and if bound to a keystroke might call (my-function dactyl count "hello")
    (=> (:right &count)
        (:insert param))))

