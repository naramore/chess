(ns chess.core
    (:require [chess.indexer :refer [not-nil?]]
              [chess.move :refer [normal-move!]]))

;; Dark (uppercase characters)
;;    | a b c d e f g h |
;; ---|-----------------|---
;;  8 | R N B K Q B N R | 8
;;  7 | P P P P P P P P | 7
;;  6 | - - - - - - - - | 6
;;  5 | - - - - - - - - | 5
;;  4 | - - - - - - - - | 4
;;  3 | - - - - - - - - | 3
;;  2 | p p p p p p p p | 2
;;  1 | r n b k q b n r | 1
;; ---|-----------------|---
;;    | a b c d e f g h |
;; Light (lowercase characters)

(def starting-board-state [\R \N \B \K \Q \B \N \R
                           \P \P \P \P \P \P \P \P
                           \- \- \- \- \- \- \- \-
                           \- \- \- \- \- \- \- \-
                           \- \- \- \- \- \- \- \-
                           \- \- \- \- \- \- \- \-
                           \p \p \p \p \p \p \p \p
                           \r \n \b \k \q \b \n \r])

(def board-state (atom nil))
(def board-history (atom nil))
(def player-state (atom nil))
(def game-log (atom nil))

(add-watch board-state :history
    (fn [_ _ _ n]
        (when-not (= (last @board-history) n)
            (swap! board-history conj n))))

(defn- record! [pos dest]
    (swap! game-log conj [pos dest]))

(defn- turn-complete! []
    (cond
        (= @player-state :light) (reset! player-state :dark)
        (= @player-state :dark) (reset! player-state :light)
        :else nil))

(defn undo! []
    (when (> (count @board-history) 1)
        (swap! board-history pop)
        (swap! game-log pop)
        (reset! board-state (last @board-history))))

(defn start-game! []
    (do (reset! game-log [])
        (reset! player-state :light)
        (reset! board-state starting-board-state)
        (reset! board-history [@board-state])))

(defn stop-game! []
    (do (reset! game-log nil)
        (reset! player-state nil)
        (reset! board-state nil)
        (reset! board-history nil)))

(defn move! [pos dest]
    (let [result (normal-move! board-state player-state pos dest)]
        (cond (not-nil? result)
              (do (record! pos dest)
                  (turn-complete!)))))