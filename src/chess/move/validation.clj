(ns chess.move.validation
	(:require [chess.indexer :as i]
            [clojure.set :refer [union]]
            [clojure.math.combinatorics :refer [cartesian-product]]))

(def walk-limit 10)

(def player-pieces {:dark  #{\R \N \B \Q \K \P}
                    :light #{\r \n \b \q \k \p}})

(def straight-shifts [[1 0]
                      [0 1]
                      [-1 0]
                      [0 -1]])

(def diagonal-shifts [[1 1]
                      [-1 1]
                      [1 -1]
                      [-1 -1]])

(def knight-shifts [[1 2] [2 1]
                    [-1 2] [2 -1]
                    [1 -2] [-2 1]
                    [-1 -2] [-2 -1]])

(def pawn-shifts {:dark [[0 -1]]
                  :light [[0 1]]})

(def pawn-attacks {:dark [[1 -1] [-1 -1]]
                   :light [[1 1] [-1 1]]})

(defn determine-player [board pos]
    (let [piece (i/lookup board pos)]
        (->> player-pieces
             (filter #((val %) piece))
             ffirst)))

(defn my-pieces? [board & positions]
    (let [pieces (map (partial i/lookup board) positions)]
        (if (and (> (count positions) 1)
                 (every? char? pieces))
            (or (every? #(Character/isUpperCase %) pieces)
                (every? #(Character/isLowerCase %) pieces))
            nil)))

(defn shift-position [[file rank] [fs rs]]
    (if (every? i/not-nil? [file rank fs rs])
        (let [new-file (char (+ (int file) fs))
              new-rank (char (+ (int rank) rs))]
            (if (and (i/valid-position? [file rank])
                     (i/valid-position? [new-file new-rank]))
                (str new-file new-rank)
                nil))
        nil))

(defn shift-walk
    ([board pos shift] (shift-walk board pos shift walk-limit))
    ([board pos shift limit]
        (loop [result #{}
               current-pos (shift-position pos shift)
               distance 0]
            (if (< distance limit)
                (cond
                    (or (nil? current-pos)
                        (my-pieces? board current-pos pos)) result
                    (= (i/lookup board current-pos) \-) (recur (conj result current-pos)
                                                               (shift-position current-pos shift)
                                                               (inc distance))
                    :else (filter i/not-nil? (conj result current-pos)))
                result))))

(defn shift-walks
  ([board pos shifts] (shift-walks board pos shifts walk-limit))
  ([board pos shifts limit] (->> (map #(shift-walk board pos % limit) shifts)
                                 (apply union))))

(defn piece-selector [board pos]
    (let [piece (i/lookup board pos)]
        (cond (char? piece) (Character/toLowerCase piece))))

(defmulti get-valid-destinations piece-selector)

(defmethod get-valid-destinations \r [board pos]
    (shift-walks board pos straight-shifts))

(defmethod get-valid-destinations \n [board pos]
    (shift-walks board pos knight-shifts 1))

(defmethod get-valid-destinations \b [board pos]
    (shift-walks board pos diagonal-shifts))

(defmethod get-valid-destinations \k [board pos]
    (shift-walks board pos (concat straight-shifts
                                   diagonal-shifts) 1))

(defmethod get-valid-destinations \q [board pos]
    (shift-walks board pos (concat straight-shifts
                                   diagonal-shifts)))

(defmethod get-valid-destinations \p [board pos]
    (let [player (determine-player board pos)
          moves (shift-walks board pos (pawn-shifts player) 1)
          attacks (shift-walks board pos (pawn-attacks player) 1)]
        (->> (concat (filter #(= (i/lookup board %) \-) moves)
                     (filter #(not= (i/lookup board %) \-) attacks))
             set)))

(defmethod get-valid-destinations :default [board pos]
    (empty #{}))

(defn get-player-piece-positions [board player]
    (->> (cartesian-product (i/valid-files) (i/valid-ranks))
         (map (partial apply str))
         (filter #(= (determine-player board %)
                     player))))

(defn get-player-moves [board player]
    (->> (get-player-piece-positions board player)
         (map #(hash-map % (get-valid-destinations board %)))
         (apply merge)
         (filter #(not-empty (val %)))
         (mapcat (fn [x] (map #(vector (key x) %) (val x))))))

(defn has-not-moved? [board-history pos]
    (->> (map #(i/lookup % pos) board-history)
         (apply =)))

(defn pawn? [board pos]
    (= (piece-selector board pos) \p))

(defn get-pawn-double-advance [board-history pos]
    (let [board (last board-history)
          player (determine-player board pos)]
        (if (and (pawn? board pos)
                 (has-not-moved? board-history pos))
            (->> (shift-walks board pos (pawn-shifts player) 2)
                 (filter #(= (i/lookup board %) \-))
                 set)
            nil)))