;; Some Questions
;; - Style questions

;; ** Is "top-down" vs "bottom-up" arrangement correct. See examples [A] below.

;; - Need to learn
;; ** Repl: How to use Repl with this module rather than standalone
;; ** Structural editing: I see the shortcuts. I need to grok them better
;; ** Macros

(ns csv_histogram.core)
(require '[clojure.string :as str])

(defn readfile
  "load file into memory as string."
  [filename]
  (slurp (str "./../../" filename)))

(defn scaled_floor
  [x & {p :precision}]
  (if p
    (let [scale (Math/pow 10 p)]
      (-> x (* scale) Math/floor (/ scale)))
    (Math/floor x)))

(defn lines [file-content] (str/split-lines file-content))

(defn pairs [lines_] (map #(str/split %1 #",") lines_))
;; How do I use shortform lambda #( % )here?
(defn pairs_with_double [pairs_] (map (fn [pair]
                                        ;; See below  for mapping a map
                                        [(nth pair 0) (Double/parseDouble (nth pair 1))]) pairs_))

(defn by-gender [pairs_] (group-by #(nth % 0) pairs_))

(defn by-bucket [sequence & [width]]
  (let [grouped (group-by
                  (fn [x]
                    (int (scaled_floor x :precision (* -1 (or width 1)))))
                  sequence)]
    (into (sorted-map) (sort-by first (seq grouped)))))

(defn counts-by-bucket [sequence] (let [b (by-bucket sequence)]
                                    (let [s (seq b)]
                                      (let [pairs (map (fn [pair]
                                                         ;; Mapping a map, use
                                                         ;;(into {} (map
                                                         ;;        (fn [[k v]] [k (function-for-transforming-value v)])
                                                         ;;        input-map))
                                                         [(nth pair 0), (count (nth pair 1))]) s)]
                                        (into (sorted-map) pairs)))))

(defn by-gender-pairs [lines_] (seq (by-gender (pairs_with_double (pairs lines_)))))

(defn gender-to-age-list [by-gender-pairs_] (map (fn [gender-and-seq]
                                                   ;; See above for mapping a map
                                                   [(nth gender-and-seq 0) (map #(nth % 1) (nth gender-and-seq 1))])
                                                 by-gender-pairs_))
(defn bucketed-by-gender [filename] (let [pairs (map (fn [gender-and-age-list]
                                                       ;; See above for mapping a map
                                                       (let [gender (nth gender-and-age-list 0)]
                                                         [gender, (counts-by-bucket (nth gender-and-age-list 1))]))
                                                     ;; [A] This function is the  top-level function that chains several other functions as in the following line..
                                                     ;; Is that good? Or does it put too much complexity right here in this function?
                                                     (gender-to-age-list (by-gender-pairs (lines (readfile filename)))))]
                                      (into (sorted-map) pairs)))

(defn hist-to-str [hist ch] (let [sorted (into (sorted-map)
                                               (sort-by first (seq hist))) pad (fn [len s]
                                                                                 (str s (str/join (repeat (max 0 (- len (count s))) " "))))]
                              (let [sq (seq sorted) pad-len 3]
                                (str/join "\n" (map #(str (pad pad-len (str (nth % 0))) " " (str/join (repeat (nth % 1) ch)))
                                                    sq)))))

(defn bucketed-by-gender-to-hist [bucketed-by-gender_]
  (let [gender_to_s (fn [gender counts] (hist-to-str counts gender))]
    (let [sq (seq bucketed-by-gender_)] (str/join "\n\n" (map (fn [pair] (gender_to_s (nth pair 0) (nth pair 1))) sq)))))

(println (bucketed-by-gender-to-hist (bucketed-by-gender "genderage.csv")))