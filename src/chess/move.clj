(ns chess.move
    (:require [chess.indexer :refer [lookup index contains-value?]]
              [chess.move.validation :refer [get-player-moves]]))

(defn castle! [king-pos rook-pos])

(defn en-passant! [pos enemy-pos])

(defn promotion! [pos dest])

(defn has-moved? [board-history pos]
    (->> (map #(lookup % pos) board-history)
         (apply =)))

(defn next-player [current-player]
	(cond
		(= current-player :light) :dark
		(= current-player :dark) :light
		:else nil))

(defn update-game [game pos dest]
	(let [piece (lookup (game :board) pos)]
		(-> (assoc-in game [:board (index pos)] \-)
			(assoc-in [:board (index dest)] piece)
			(assoc :player (next-player (game :player)))
			(assoc-in [:log (count (game :log))] [pos dest]))))

(defn normal-move! [game-atom pos dest]
    (let [board (@game-atom :board)
    	  player (@game-atom :player)]
        (if (contains-value? (get-player-moves board player) [pos dest])
        	(swap! game-atom update-game pos dest)
            nil)))