;;;https://github.com/opqdonut/clojure-exercises
(ns exercises.tests1
  (:require [clojure.string :as str]))

(defn combine [seq-maps] (reduce merge {} seq-maps))
(defn assertions []
  (assert (= 2 (+ 1 1)))
  (assert (= \i (get "ciao" 1)))
  (assert (= :treasure (let [val {:description "cave"
                                  :crossroads  [{:contents :monster}
                                                nil
                                                {:contents [:trinket :treasure]}]}]
                         (get-in val '(:crossroads 2 :contents 1)))))
  (assert (= {:a 1 :b 2 :c 3 :d 4 :e 5} (combine [{:a 1 :b 2} {:c 3} {:d 4 :e 5}])))
  )

(assertions)