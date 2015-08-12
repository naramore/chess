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
    (do
        (reset! game-log [])
        (reset! player-state :light)
        (reset! board-state starting-board-state)))

(defn stop-game! []
    (do
        (reset! game-log nil)
        (reset! player-state nil)
        (reset! board-state nil)))

(defn move! [pos dest]
    (let [[px py] pos
          [dx dy] dest
          piece (i/lookup @board-state pos)
          dest-piece (i/lookup @board-state dest)]
        (if (i/contains-value? (p/get-valid-destinations @board-state pos) dest)
            (do
                (swap! board-state assoc (i/index px py) \-)
                (swap! board-state assoc (i/index dx dy) piece))
            nil)))

(defn castle! [king-pos rook-pos])

(defn en-passant! [pos enemy-pos])

(defn promotion! [pos dest])

(defn check [board])

(def generate-board-edge
    (let [width (count (i/valid-files))]
        (->> (repeat 8 "---|")
             (apply concat)
             (concat "|")
             (apply str))))

(defn generate-board-rank [rank]
    (->> (mapcat #(str " " (if (= % \-) \space %) " |") rank)
         (concat "|")
         (apply str)))

(defn generate-board [board]
    (->> (partition (count (i/valid-files)) board)
         (map generate-board-rank)
         (interpose generate-board-edge)
         (cons generate-board-edge)
         (#(concat % [generate-board-edge]))
         ((partial clojure.string/join "\n"))))

(defn print-board! []
    (if (nil? @board-state)
        (->> (repeat 64 \space)
             (apply vector)
             generate-board
             println)
        (->> (generate-board @board-state)
             println)))