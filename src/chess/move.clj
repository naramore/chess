(ns chess.move
    (:require [clojure.set :refer [union difference]]
              [chess.indexer :refer [lookup index contains-value? valid-ranks]]
              [chess.move.validation :refer [get-player-moves pawn? player-pieces]]))

(defn castle! [king-pos rook-pos])

(defn en-passant! [pos enemy-pos])

(defn valid-promotions [player]
    (difference (player-pieces player) #{\p \k \P \K}))

(defn promote? [board pos dest]
    (and (pawn? board pos)
         (or (= (first (valid-ranks)) (second dest))
             (= (last (valid-ranks)) (second dest)))))

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

(defn normal-move!
    ([game-atom history-atom pos dest]
        (normal-move! game-atom pos dest nil))
    ([game-atom history-atom pos dest promotion]
        (let [board (@game-atom :board)
        	  board-history (map :board @history-atom)
              player (@game-atom :player)]
            (cond (contains-value? (get-player-moves board-history player) [pos dest])
                (if (promote? board pos dest)
                    (cond ((valid-promotions player) promotion)
                          (swap! game-atom update-game pos dest promotion))
                    (swap! game-atom update-game pos dest))))))