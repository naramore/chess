(ns chess.core
    (:require [chess.indexer :refer [not-nil?]]
              [chess.move :refer [normal-move!]]
              [chess.parser :refer [parse-move]]))

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

(def empty-board (vec (repeat 64 \-)))

(def starting-board [\R \N \B \K \Q \B \N \R
                     \P \P \P \P \P \P \P \P
                     \- \- \- \- \- \- \- \-
                     \- \- \- \- \- \- \- \-
                     \- \- \- \- \- \- \- \-
                     \- \- \- \- \- \- \- \-
                     \p \p \p \p \p \p \p \p
                     \r \n \b \k \q \b \n \r])

(def game-state (atom nil))
(def game-history (atom nil))

(add-watch game-state :history
    (fn [_ _ _ n]
        (when-not (= (last @game-history) n)
            (swap! game-history conj n))))

(defn undo! []
    (when (> (count @game-history) 1)
        (swap! game-history pop)
        (reset! game-state (last @game-history))))

(defn start-game! []
    (do (reset! game-state {:board starting-board
                            :player :light
                            :log []})
        (reset! game-history [@game-state])))

(defn stop-game! []
    (do (reset! game-state nil)
        (reset! game-history nil)))

(defn move!
    ([m] (->> (parse-move m)
              (apply move!)))
    ([pos dest] (move! pos dest nil))
    ([pos dest promotion] (normal-move! game-state pos dest promotion)))