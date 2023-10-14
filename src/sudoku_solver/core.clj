(ns sudoku-solver.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def input (slurp (io/resource "input.txt")))

(def board (partition-all 9 (filter #(re-matches #"[\d\.]" (str %))
                                    input)))

(defn get-columns [board]
  (apply mapv vector board))

(defn get-quads [board]
  (for [x [0 3 6]
        y [0 3 6]]
    (apply concat (map #(take 3 (drop y %)) (take 3 (drop x board))))))

(defn no-dup-elements [row]
  (let [elements (filter
                   #(not= % \.)
                   row)]
    (= (count elements)
       (count (set elements)))
    #_(apply distinct? elements)))


(deftest tests
  (testing
    "check for no duplicates"
    (is
      (false?
        (no-dup-elements [\1 \3 \9 \6 \5 \7 \4 \8 \2 \2])))
    (is
      (true?
        (no-dup-elements [\1 \3 \9 \6 \5 \7 \4 \8 \2])))))



(defn no-duplicates? [rows]
  (every? no-dup-elements rows))

(defn legit-board? [board]
  (and
    (no-duplicates? board)
    (no-duplicates? (get-columns board))
    (no-duplicates? (get-quads board))))

(defn row-complete? [row]
  (every?
    #(not= % \.)
    row))

(defn board-complete? [board]
  (every? row-complete? board))

(defn get-next-digits [row]
  (get (group-by
         (set (filter
                #(not= % \.)
                row))

         (map #(first (str %))
              (range 1 10))) nil))

(defn find-first
  [s d]
  (reduce (fn [i x]
            (if (= x d) (reduced i) (inc i)))
          0
          s))

(defn find-empty-slot
  "returns the index of the first empty slot in the row"
  [row]
  (find-first row \.))

(defn all-next-steps-in-row
  "all possible next steps in this row"
  [row]
  (let [possible-digits (get-next-digits row)
        next-slot       (find-empty-slot row)]
    (map
      (fn [attempt]
        (assoc (vec row) next-slot attempt))
      possible-digits)))

(defn next-boards [board row-num]
  (map
    (fn [new-row] (assoc (vec board) row-num new-row))
    (all-next-steps-in-row (nth board row-num))))

(defn find-first-incomplete-row
  "Given an (incomplete) board, looks for the first row that is still to be filled in"
  [board]
  (ffirst (filter
            #((complement row-complete?) (second %))
            (map vector (range) board))))

(filter
  (set (filter
         #(not= % \.)
         (first board)))
  (map #(first (str %))
       (range 9)))

(defn iter
  "takes a board, returns the next possible steps.
  will fill the top-most empty row first"
  [board]
  (let [results (filter
                  legit-board?
                  (next-boards
                    board (find-first-incomplete-row board)))]
    results))


(defn solve [board]
  (loop [states [board]]
    ;(println (count states))

    (if (seq states)
      (let [board (first states)]
        (if (board-complete? board)
          board
          (recur
            (concat
              (iter board)
              (rest states))))))))

(def test-board
  [[\5 \3 \4 \6 \7 \8 \9 \1 \2]
   [\6 \. \. \1 \9 \5 \. \. \.]
   [\. \9 \8 \. \. \. \. \6 \.]
   [\8 \. \. \. \6 \. \. \. \3]
   [\4 \. \. \8 \. \3 \. \. \1]
   [\7 \. \. \. \2 \. \. \. \6]
   [\. \6 \. \. \. \. \2 \8 \.]
   [\. \. \. \4 \1 \9 \. \. \5]
   [\. \. \. \. \8 \. \. \7 \9]])

(def test-board-2
  [[\. \4 \. \1 \. \. \3 \. \.]
   [\. \. \. \. \7 \. \. \9 \.]
   [\9 \3 \. \. \. \. \. \5 \1]

   [\. \. \8 \. \. \. \. \. \5]
   [\. \. \3 \2 \. \6 \. \. \.]
   [\. \. \2 \. \8 \. \4 \. \.]

   [\5 \. \. \9 \6 \1 \. \. \.]
   [\. \. \. \5 \3 \. \. \. \.]
   [\. \. \. \. \. \8 \. \. \7]])

(def test-board-samurai
  [[\. \8 \. \. \. \. \9 \7 \.]
   [\. \. \4 \. \. \. \. \. \.]
   [\. \. \. \. \. \9 \3 \. \4]

   [\. \6 \. \. \7 \. \. \3 \.]
   [\. \1 \. \6 \2 \8 \. \4 \.]
   [\. \7 \. \. \3 \. \. \8 \.]

   [\6 \. \3 \2 \. \. \. \9 \8]
   [\. \. \. \. \. \. \. \5 \.]
   [\. \4 \1 \. \. \. \2 \. \.]])

(def test-tough-board
  [[\. \. \. \6 \. \. \. \8 \.]
   [\. \. \. \3 \. \. \. \. \1]
   [\. \7 \. \. \. \. \3 \. \5]
   [\3 \. \. \. \. \. \. \. \.]
   [\. \. \. \. \. \. \9 \5 \8]
   [\. \. \2 \4 \. \1 \. \. \.]
   [\. \. \7 \. \. \3 \. \. \.]
   [\. \6 \1 \. \. \9 \. \. \3]
   [\4 \8 \. \. \. \. \2 \. \.]])


;; Add ability to read in sudoku puzzles from the command line
;; one line at a time

(defn read-sudoku []
  (for [x (range 9)]
    (loop []
      (let [in     (read-line)
            digits (filter #(not= \space %) in)]
        (if (= (count digits) 9)
          (vec digits)
          (do
            (println "You did not enter 9 digits, try again.")
            (recur)))))))
(test (clojure.test/is (= (solve test-tough-board)

                          [[\1 \3 \9 \6 \5 \7 \4 \8 \2]
                           [\6 \4 \5 \3 \8 \2 \7 \9 \1]
                           [\2 \7 \8 \9 \1 \4 \3 \6 \5]
                           [\3 \9 \6 \5 \7 \8 \1 \2 \4]
                           [\7 \1 \4 \2 \3 \6 \9 \5 \8]
                           [\8 \5 \2 \4 \9 \1 \6 \3 \7]
                           [\9 \2 \7 \8 \4 \3 \5 \1 \6]
                           [\5 \6 \1 \7 \2 \9 \8 \4 \3]
                           [\4 \8 \3 \1 \6 \5 \2 \7 \9]])))