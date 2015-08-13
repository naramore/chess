(ns chess.print
    (:require [chess.indexer :refer [valid-files valid-ranks]]))

(def board-width (count (valid-files)))
(def board-hight (count (valid-ranks)))
(def board-size (* board-width board-hight))

(def generate-board-edge
    (->> (repeat board-width "---|")
         (apply concat)
         (concat "|")
         (apply str)))

(defn generate-board-rank [rank]
    (->> (mapcat #(str " " (if (= % \-) \space %) " |") rank)
         (concat "|")
         (apply str)))

(defn generate-board [board]
    (->> (partition board-width board)
         (map generate-board-rank)
         (interpose generate-board-edge)
         (cons generate-board-edge)
         (#(concat % [generate-board-edge]))
         ((partial clojure.string/join "\n"))))

(defn print-board [board]
    (if (nil? board)
        (->> (repeat board-size \space)
             (apply vector)
             generate-board
             println)
        (->> (generate-board board)
             println)))