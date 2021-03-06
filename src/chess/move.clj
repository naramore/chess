(ns chess.move
    (:require [clojure.set :refer [union difference intersection]]
              [chess.indexer :refer [lookup index contains-value? valid-ranks]]
              [chess.move.validation :refer [get-player-piece-positions get-player-moves pawn? player-pieces]]))

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
    ([game history pos dest]
        (let [piece (lookup (game :board) pos)]
            (update-game game history pos dest piece)))
    ([game history pos dest piece]
        (-> (assoc-in game [:board (index pos)] \-)
            (assoc-in [:board (index dest)] piece)
            (assoc :player (next-player (:player game)))
            (assoc-in [:log (count (:log game))] [pos dest])
            (#(assoc % :moves (get-player-moves (conj (map :board history) (:board %))
                                                (next-player (:player game))))))))

(defn my-king-position [board player]
    (->> (get-player-piece-positions board player)
         (filter #(#{\k \K} (lookup board %)))
         first))

(defn check? [board-history player]
    (let [king (my-king-position (last board-history) player)
          enemy (next-player player)]
        (->> (get-player-moves board-history enemy)
             (filter #(= (second %) king))
             not-empty)))

(defn normal-move!
    ([game-atom history-atom pos dest]
        (let [player (:player @game-atom)
              piece (intersection #{\q \Q} (player-pieces player))]
            (normal-move! game-atom history-atom pos dest (first piece))))
    ([game-atom history-atom pos dest promotion]
        (let [board (@game-atom :board)
        	    board-history (map :board @history-atom)
              player (@game-atom :player)]
            (cond ((:moves @game-atom) [pos dest])
                (if (promote? board pos dest)
                    (cond ((valid-promotions player) promotion)
                          (swap! game-atom update-game @history-atom pos dest promotion))
                    (swap! game-atom update-game @history-atom pos dest))))))