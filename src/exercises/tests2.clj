;;https://arielortiz.info/apps/s201911/tc2006/activity_simple_exercises/

(ns exercises.tests2

  (:require [clojure.test :refer [deftest is testing run-tests]])
  (:require [clojure.math.numeric-tower :refer [sqrt]])
  )
(defn sign [x]
  (cond
    (< x 0) -1
    (> x 0) 1
    :else 0)
  )
(deftest test-sign
  (is (= -1 (sign -5)))
  (is (= 1 (sign 10)))
  (is (= 0 (sign 0)))
  )

(defn roots [a b c]
  (let [discriminant (sqrt (- (* b b) (* 4 a c)))
        minusb (- b)
        twoa (* 2 a)]
    [
     (/ (+ minusb discriminant) twoa) (/ (- minusb discriminant) twoa)
     ]
    )

  )
(deftest test-roots
  (is (= [-1 -1] (roots 2 4 2)))
  (is (= [0 0] (roots 1 0 0)))
  (is (= [-1/4 -1] (roots 4 5 1)))
  )