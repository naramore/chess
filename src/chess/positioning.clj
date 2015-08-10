(ns chess.positioning
	(:require [chess.indexer :as i]))

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

(def not-nil? (complement nil?))

(defn shift-position [[file rank] [fs rs]]
    (let [new-file (char (+ (int file) fs))
          new-rank (char (+ (int rank) rs))]
        (if (and (i/valid-position? [file rank])
                 (i/valid-position? [new-file new-rank]))
            (str new-file new-rank)
            nil)))

(defn shift-walk
    ([board pos shift] (shift-walk board pos shift 10))
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
                    :else (filter not-nil? (conj result current-pos)))
                result))))

(defn shift-walks
  ([board pos shifts] (shift-walks board pos shifts 10))
  ([board pos shifts limit] (mapcat #(shift-walk board pos % limit) shifts)))

(defn my-pieces? [board & positions]
    (if (and (> (count positions) 1))
        (let [pieces (map #(i/lookup board %) positions)]
            (or (every? #(Character/isUpperCase %) pieces)
                (every? #(Character/isLowerCase %) pieces)))
        nil))

(defmulti get-valid-destinations (fn [board pos] (i/lookup board pos)))

(defmethod get-valid-destinations #{\r \R} [board pos]
    (shift-walks board pos straight-shifts))

(defmethod get-valid-destinations #{\n \N} [board pos]
    (shift-walks board pos knight-shifts 1))

(defmethod get-valid-destinations #{\b \B} [board pos]
    (shift-walks board pos diagonal-shifts))

(defmethod get-valid-destinations #{\k \K} [board pos]
    (shift-walks board pos (concat straight-shifts
                                   diagonal-shifts) 1))

(defmethod get-valid-destinations #{\q \Q} [board pos]
    (shift-walks board pos (concat straight-shifts
                                   diagonal-shifts)))

(defmethod get-valid-destinations #{\p \P} [board pos]
    (empty []))

(defmethod get-valid-destinations #{\- nil :default} [board pos]
    (empty []))