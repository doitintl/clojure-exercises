;; Some Questions
;; - Style questions

;; ** Is "top-down" vs "bottom-up" arrangement correct. See examples [A] below.

;; - Need to learn
;; ** Repl: How to use Repl with this module rather than standalone
;; ** Structural editing: I see the shortcuts. I need to grok them better
;; ** Macros

(ns csv-histogram.core (:require  [clojure.string :as str]))

(defn readfile
  "load file into memory as string."
  [filename]
  (slurp (str "./../../" filename)))

(defn scaled-floor
  [x & {p :precision}]
  (if p
    (let [scale (Math/pow 10 p)]
      (-> x (* scale) Math/floor (/ scale)))
    (Math/floor x)))

(defn lines [file-content] (str/split-lines file-content))

(defn pairs [lines] (map #(str/split %1 #",") lines))

(defn pairs-with-double [gender-and-num-as-str] (map (fn [[k v]]
                                                       [k (Double/parseDouble v)]) gender-and-num-as-str))

(defn by-gender [pairs] (group-by #(first %) pairs))

(defn by-bucket [sequence & [width]]  (->> sequence
                                           (group-by (fn [x]
                                                       (int (scaled-floor x :precision (* -1 (or width 1))))))
                                           (into {})))

(defn counts-by-bucket [sequence] (let [b (by-bucket sequence) pairs (map (fn [[k, v]]
                                                                            [k, (count v)]) b)]
                                    (into {} pairs)))

(defn by-gender-pairs [lines] (by-gender (pairs-with-double (pairs lines))))

(defn gender-to-age-list [gender-and-stats] (map (fn [[gender sequence]]
                                                   [gender (map #(nth % 1) sequence)])
                                                 gender-and-stats))
(defn bucketed-by-gender [filename] (let [pairs (map (fn [[gender age-list]]
                                                       [gender (counts-by-bucket age-list)])
                                                     (gender-to-age-list (by-gender-pairs (lines (readfile filename)))))]
                                      (into  {}  pairs)))

(defn hist-to-str [hist ch] (let [sorted (into (sorted-map) ;;sorted-map needed to preserve order given in the next form
                                               (sort-by first hist)) pad (fn [len s]
                                                                           (str s (str/join (repeat (max 0 (- len (count s))) " "))))]
                              (let [pad-len 3]
                                (str/join "\n" (map #(str (pad pad-len (str (first %))) " " (str/join (repeat (nth % 1) ch)))
                                                    sorted)))))

(defn bucketed-by-gender-to-hist [bucketed-by-gender]
  (let [gender-to-s (fn [gender counts] (hist-to-str counts gender))]
    (str/join "\n\n" (map (fn [[gender  stats]] (gender-to-s gender stats)) bucketed-by-gender))))

(println (bucketed-by-gender-to-hist (bucketed-by-gender "genderage.csv")))