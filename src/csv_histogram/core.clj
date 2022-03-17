;;  ?? Need to learn
;; ** Repl: How to use Repl with this module rather than standalone
;; ** Structural editing: I see the shortcuts. I need to grok them better
;; ** Macros
;;

(ns csv-histogram.core
  (:require [clojure.string :as str]))

(defn scaled-floor [x & {precision :precision}]
  "Round down. precision indicates decimal points precison, so that 2 means round 2.222 to 2.22 and -2 means round 2222 to 2200"
  (if precision
    (let [scale (Math/pow 10 precision)]
      (-> x (* scale) Math/floor (/ scale)))
    (Math/floor x)))


(defn readfile [filename]
  "Load file into memory as string. All IO is here"
  (slurp (str "./../../" filename)))


(defn parse [file-content]
  "Convert raw input into basic data structure"
  (let [split-to-lines (fn [file-content] (str/split-lines file-content))
        comma-separate-pairs (fn [lines] (map #(str/split % #",") lines))
        gender-and-num (fn [gender-and-num-as-str] (map (fn [[gender num-as-str]]
                                                          [gender (Double/parseDouble num-as-str)]) gender-and-num-as-str))
        ]
    (-> file-content (split-to-lines) (comma-separate-pairs) (gender-and-num))
    )
  )



(defn group-by-bucket [ages & [bucket-width]]
  "Take a sequence of numbers and bucket them by bucket-width. Default bucket-width is -1, i.e., round down to the nearest 10."
  (->> ages
       (group-by (fn [x]
                   (int (scaled-floor x :precision (* -1 (or bucket-width 1))))))
       (into {})))

(defn counts-by-bucket [gender-and-ages]
  "Take a map which maps gender to a sequence of numbers, and
  return a map which maps genders to a map of buckets (indexed by the bottom of the bucket) to a  count of ages in that bucket"
  (->> gender-and-ages (group-by-bucket) (map (fn [[gender, ages]] [gender, (count ages)])) (into {})))


(defn gender-to-age-list [gender-and-gender-and-age-pairs]
  "Just cleanup. Take a map which maps gender to a sequence of pairs of [gender, age] (where the first element of that
  pair is clearly redundant),
  and return a map which maps gender to a sequence of ages."
  (map (fn [[gender gender-and-age-pairs]]
         [gender (map #(second %) gender-and-age-pairs)])
       gender-and-gender-and-age-pairs))

(defn bucketed-by-gender [filename]
  "Given the file, create the full data-structure that will be converted to a histogram"
  (let [
        group-by-gender (fn [gender-and-stats] (group-by #(first %) gender-and-stats))
        pairs (map (fn [[gender age-list]]
                     [gender (counts-by-bucket age-list)])
                   (gender-to-age-list (group-by-gender (parse (readfile filename)))))]
    (into {} pairs)))



(defn bucketed-by-gender-to-hist [bucketed-by-gender]
  (let [pad-len 3
        histogram-data-to-str (fn [histogram-data char]
                                (let [
                                      sorted (into (sorted-map) histogram-data) pad (fn [len s]
                                                                                      (str s (str/join (repeat (max 0 (- len (count s))) " "))))]
                                  (str/join "\n" (map #(str (pad pad-len (str (first %))) " " (str/join (repeat (second %) char))) sorted))))
        one-gender-histogram-to-string (fn [gender counts] (histogram-data-to-str counts gender))
        ]
    (str/join "\n\n" (map (fn [[gender stats]] (one-gender-histogram-to-string gender stats)) bucketed-by-gender))))

(println (bucketed-by-gender-to-hist (bucketed-by-gender "genderage.csv")))