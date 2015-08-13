(ns chess.move
    (:require [clojure.set :refer [difference]]
              [chess.indexer :refer [lookup index contains-value? valid-ranks]]
              [chess.move.validation :refer [get-player-moves pawn? player-pieces]]))

(defn castle! [king-pos rook-pos])

(defn en-passant! [pos enemy-pos])

(defn next-player [current-player]
	(cond
		(= current-player :light) :dark
		(= current-player :dark) :light
		:else nil))

(defn update-game
    ([game pos dest]
        (let [piece (lookup (game :board) pos)]
            (update-game game pos dest piece)))
    ([game pos dest piece]
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

(defn promote? [board pos dest]
    (and (pawn? board pos)
         (or (= (first (valid-ranks)) (second dest))
             (= (last (valid-ranks)) (second dest)))))

(defn promotion! [game-atom pos dest promotion])