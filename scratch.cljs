(ns user
  (:require [clojure.test :refer [is testing]]))
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

;; SEARCH
;; c03 linear search

;; now using https://github.com/ThePrimeagen/kata-machine
;; but we copy the test here

;; clojure note: no early return on clojure

(defn linear-search [haystack needle]
  (loop [i 0]
    (if (< i (count haystack))
      (if (= (get haystack i) needle)
        true
        (recur (inc i)))
      false)))

(let [foo [1, 3, 4, 69, 71, 81, 90, 99, 420, 1337, 69420]]
  (testing "linear search array"
    (is (= (linear-search foo 69) true))
    (is (= (linear-search foo 1336) false))
    (is (= (linear-search foo 69420) true))
    (is (= (linear-search foo 69421) false))
    (is (= (linear-search foo 1) true))
    (is (= (linear-search foo 0) false))))

;; binary search algorithm

;; doozy, he said, off-by-one? he said

;; question to ask: is it ordered?
;; if yes => guess where to start searching!
;; 
;; halve, halve, halve! until it's 1
;; how many halving? N/2^k = 1 ==> k = log n
;; O(logN)

;; BigO trick => halving? might be O(logN) or O(NlogN)

(defn binary-search [haystack needle]
  (-> (loop [lo 0 hi (count haystack)]
        (when (< lo hi)
          (let [mid (Math/floor (+ lo (/ (- hi lo) 2)))
                v   (get haystack mid)]
            (if (= needle v)
              true
              (if (> needle v)
                (recur (inc mid) hi)
                (recur lo mid))))))
      (boolean)))


(let [foo [1, 3, 4, 69, 71, 81, 90, 99, 420, 1337, 69420]]
  (testing "binary-search array"
    (is (= (binary-search foo 69) true))
    (is (= (binary-search foo 1336) false))
    (is (= (binary-search foo 69420) true))
    (is (= (binary-search foo 69421) false))
    (is (= (binary-search foo 1) true))
    (is (= (binary-search foo 0) false))))

;; 2 Crystal Ball

(defn two-crystal-balls [data]
  (let [size    (count data)
        jmp-amt (Math/floor (Math/sqrt size))
        first-break
        ,, (loop [i 0]
             (if (< i size)
               (if (true? (get data i))
                 i
                 (recur (+ i jmp-amt)))
               -1))]
    (if (= first-break -1)
      -1
      (loop [i (inc (- first-break jmp-amt))]
        (if (<= i first-break)
          (if (true? (get data i))
            i
            (recur (inc i)))
          -1)))))
;; first try baby!

;; note on why sqrt of N => improving O(N) to O(√N)

(defn two-crystal-balls-clj [data]
  (let [jmp-amt (Math/floor (Math/sqrt (count data)))
        [[first-break-idx remaining-search]]
        ,, (into []
                 (comp (map-indexed (fn [idx el] [idx el]))
                       (filter (fn [[_ el]] (some true? el)))
                       (take 1))
                 (partition jmp-amt data))
        [[remaining-idx _] :as linear-search-res]
        ,, (into []
                 (comp (map-indexed (fn [idx el] [idx el]))
                       (filter (fn [[_ el]] (true? el)))
                       (take 1))
                 remaining-search)]
    (if (empty? linear-search-res) -1
        (+ (* first-break-idx jmp-amt) remaining-idx))))

(let [size 10000
      idx  (Math/floor (* (Math/random) size))
      data (.. (js/Array. size) (fill false))]
  (loop [i idx]
    (when (< i size)
      (aset data i true)
      (recur (inc i))))

  (testing "two crystal balls"
    (is (= (two-crystal-balls data) idx))
    (is (= (two-crystal-balls (.. (js/Array. 821) (fill false))) -1))
    (is (= (two-crystal-balls-clj data) idx))
    (is (= (two-crystal-balls-clj (.. (js/Array. 821) (fill false))) -1))))

(let [size 10000
      idx  (Math/floor (* (Math/random) size))
      data (.. (js/Array. size) (fill false))]
  (loop [i idx]
    (when (< i size)
      (aset data i true)
      (recur (inc i))))
  (time
   (two-crystal-balls data)
   #_(two-crystal-balls-clj data) #_"clj is slower here"))

;; Sorting

;; sorted array is..
;; any arr[i] <= arr[i+1] 

;; 1. BubbleSort

;; bubble sort needs n , n - 1, n - 2 ... n - n + 1 iterations
;; what is this running time
;; let me tell you the tale of an asshole named Gauss
;; summing all the number from 1..100
;; ...
;; (n + 1) * (n / 2)
;; O(n^2)

;; my take
(defn bubble-sort [arr]
  (loop [i 0 a (transient arr)
         last-i (dec (count a))
         swap-occur? false]
    (if (< i last-i)
      (let [curr-v (nth a i) next-v (nth a (inc i))
            do-swap? (> curr-v next-v)]
        (recur (inc i)
               (cond-> a
                 do-swap? (-> (assoc! i next-v)
                              (assoc! (inc i) curr-v)))
               last-i (or swap-occur? do-swap?)))
      (if swap-occur?
        (recur 0 a (dec last-i) false)
        (persistent! a)))))

;; second take, following primeagen's typescript
(defn bubble-sort-primeagen [arr]
  (let [a (transient arr)]
    (loop [i 0 a a]
      (if (< i (count a))
        (recur
         (inc i)
         (loop [j 0 a a]
           (if (< j (- (count a) 1 i))
             (recur
              (inc j)
              (let [curr-v (nth a j) next-v (nth a (inc j))
                    do-swap? (> curr-v next-v)]
                (cond-> a
                  do-swap? (-> (assoc! j next-v)
                               (assoc! (inc j) curr-v)))))
             a)))
        (persistent! a)))))

(for [i (range 10)]
  (inc i))

(let [arr      [9, 3, 7, 4, 69, 420, 42]
      expected [3, 4, 7, 9, 42, 69, 420]]
  (testing "bubble sort"
    (is (= (bubble-sort arr) expected))
    (is (= (bubble-sort-primeagen arr) expected))))
