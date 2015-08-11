(ns chess.core
    (:require [chess.indexer :as i]
              [chess.positioning :as p]))

;; Chess Board Data Structure:
;; ----------------------------
;;
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

(def board-state (atom nil))
(def player-state (atom nil))
(def game-log (atom nil))

(def starting-board-state [\R \N \B \K \Q \B \N \R
                           \P \P \P \P \P \P \P \P
                           \- \- \- \- \- \- \- \-
                           \- \- \- \- \- \- \- \-
                           \- \- \- \- \- \- \- \-
                           \- \- \- \- \- \- \- \-
                           \p \p \p \p \p \p \p \p
                           \r \n \b \k \q \b \n \r])

(defn start-game! []
    (reset! board-state starting-board-state))

(defn stop-game! []
    (reset! board-state nil))

(defn move! [pos dest])

(defn castle! [king-pos rook-pos])

(defn en-passant! [pos enemy-pos])

(defn promotion! [pos dest])

(defn check [board])



(defn move [board pos dest]
    (let [[px py] pos
          [dx dy] dest
          piece (i/lookup board pos)
          dest-piece (i/lookup board dest)]
        (if (i/contains-value? (p/get-valid-destinations board pos) dest)
            (do
                (assoc board (i/index px py) \-)
                (assoc board (i/index dx dy) piece))
            nil)))
