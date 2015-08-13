(ns chess.move
    (:require [chess.indexer :refer [lookup index contains-value?]]
              [chess.move.validation :refer [get-player-moves]]))

(defn normal-move! [board-atom player-atom pos dest]
    (let [piece (lookup @board-atom pos)]
        (if (contains-value? (get-player-moves @board-atom @player-atom) [pos dest])
        	(swap! board-atom assoc (index pos) \-
                                    (index dest) piece)
            nil)))

(defn castle! [king-pos rook-pos])

(defn en-passant! [pos enemy-pos])

(defn promotion! [pos dest])

(defn has-moved? [board-history pos]
    (->> (map #(lookup % pos) board-history)
         (apply =)))