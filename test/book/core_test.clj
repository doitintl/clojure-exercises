(ns book.core-test
  (:require [clojure.test :refer :all]
            [book.core :refer :all]))

(deftest a-test
  (testing "Search for books by substring of title."
    (is (not-any? #(= "Herman Melville" (:author %)) (search "not a book" catalog))))
  (is (= (count (search "not a book" catalog)) 0))
  (is (= (count (search "and" catalog)) 3))
  )
