(ns chess.print
    (:require [chess.indexer :refer [valid-files valid-ranks]]))

(def board-width (count (valid-files)))
(def board-hight (count (valid-ranks)))
(def board-size (* board-width board-hight))

(defn generate-board-edge
    ([] (generate-board-edge "---"))
    ([s] (->> (repeat board-width (str s "|"))
              (apply str)
              (str "  |"))))

(defn generate-board-rank
    ([rank] (generate-board-rank nil rank))
    ([s rank] (generate-board-rank s "|" rank))
    ([s div rank]
        (->> (mapcat #(str " " (if (= % \-) \space %) " " div) rank)
             (concat s div)
             (apply str))))

(defn generate-board [board]
    (->> (partition board-width board)
         (map generate-board-rank (map #(str % " ") (reverse (valid-ranks))))
         (interpose (generate-board-edge))
         (cons (generate-board-edge))
         (#(concat % [(generate-board-edge)
                      (generate-board-rank "  " " " (valid-files))]))
         ((partial clojure.string/join "\n"))))

(defn print-board [board]
    (if (nil? board)
        (->> (repeat board-size \space)
             (apply vector)
             generate-board
             println)
        (->> (generate-board board)
             println)))