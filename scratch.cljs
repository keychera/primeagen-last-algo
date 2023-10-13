(ns user
  (:require [clojure.test :refer [is]]))
;; for learning https://frontendmasters.com/courses/algorithms/
;; clojure might be not the best suited for this
;; this is just notes and for fun

(def a [])
(vector? a)

;; c01 BigO
;; https://frontendmasters.com/courses/algorithms/big-o-time-complexity/

(defn sum-char-codes [s]
  (->> s (map int) (reduce + 0)))

(sum-char-codes "hello!")

;; O(N)
;; growth is with respect to the input
;; always drop constant
;; practical vs theoretical diff, smaller input might matter

(defn sum-char-codes-without-E [s]
  (loop [cur (first s) rem (rest s) sum 0]
    (if (= cur \E) sum
        (if (empty? rem)
          (+ sum (int cur))
          (recur (first rem) (rest rem) (+ sum (int cur)))))))

(is (= (sum-char-codes "hello!")
       (sum-char-codes-without-E "hello!")
       (sum-char-codes-without-E "hello!Eee")))

;; still O(N)
;; often consider worst case

;; maybe N^2 
(defn sum-char-codes-O-N-squared [s]
  (->> s
       (mapv (fn [_] (mapv int s)))
       (flatten)
       (reduce + 0)))

(sum-char-codes-O-N-squared "hello!")

;; O(N^2)
;; O(N^3) , matrices calc might have this
;; O(n log n)
;; O(log n)
;; O(sqrt(n))


;; there are stuff other than Big O
;; Space complexity not included

;; c02 Arrays Data Structure

;; array: contiguous memory space
;; a = int[3]
;; a[0] ==> go to memory address of a + offset of 0 * how big the type is

;; wait a MINUTE, we can use nbb! :O

(def arr (js/ArrayBuffer. 6))
(js/console.log arr) ;; println in the terminal, not output.calva-repl


(def arr8 (js/Uint8Array. arr))
(aset arr8 2 45)
(js/console.log arr)

(def arr16 (js/Uint16Array. arr))
(aset arr16 2 0x4545)
(js/console.log arr)

;; array = getting, insertion, deletion at specific idx
;;         fixed size, contiguous
