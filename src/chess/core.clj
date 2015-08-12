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
(def board-history (atom nil))
(def player-state (atom nil))
(def game-log (atom nil))

(add-watch board-state :history
    (fn [_ _ _ n]
        (when-not (= (last @board-history) n)
            (swap! board-history conj n))))

(defn undo! []
    (when (> (count @board-history) 1)
        (swap! board-history pop)
        (reset! board-state (last @board-history))))

(def starting-board-state [\R \N \B \K \Q \B \N \R
                           \P \P \P \P \P \P \P \P
                           \- \- \- \- \- \- \- \-
                           \- \- \- \- \- \- \- \-
                           \- \- \- \- \- \- \- \-
                           \- \- \- \- \- \- \- \-
                           \p \p \p \p \p \p \p \p
                           \r \n \b \k \q \b \n \r])

(defn record! [pos dest]
    (swap! game-log conj [pos dest]))

(defn turn-complete! []
    (cond
        (= @player-state :light) (reset! player-state :dark)
        (= @player-state :dark) (reset! player-state :light)
        :else nil))

(defn start-game! []
    (do
        (reset! game-log [])
        (reset! player-state :light)
        (reset! board-state starting-board-state)
        (reset! board-history [@board-state])))

(defn stop-game! []
    (do
        (reset! game-log nil)
        (reset! player-state nil)
        (reset! board-state nil)
        (reset! board-history nil)))

(defn move! [pos dest]
    (let [piece (i/lookup @board-state pos)]
        (if (i/contains-value? (p/get-player-moves @board-state @player-state) [pos dest])
            (do
                (swap! board-state assoc (i/index pos) \-
                                         (i/index dest) piece)
                (record! pos dest)
                (turn-complete!))
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

(defn print-board [board]
    (if (nil? board)
        (->> (repeat 64 \space)
             (apply vector)
             generate-board
             println)
        (->> (generate-board board)
             println)))