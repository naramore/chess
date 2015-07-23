(ns chess.positioning
	(:require [[chess.indexer :as i]]))

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

(defn contains-value? [coll val]
    (some (-> val vector set) coll))

(def not-nil? (complement nil?))

(defn shift-position [[file rank] [fs rs]]
    (let [new-file (char (+ (int file) fs))
          new-rank (char (+ (int rank) rs))]
        (if (and (contains-value? (i/valid-files) new-file)
                 (contains-value? (i/valid-ranks) new-rank))
            (str new-file new-rank)
            nil)))

;; FIX THIS FUNCTION TO NOT INCLUDE HITTING YOUR OWN PIECES
;; ----------------------------------------------------------
;; nil      -> don't add, return
;; \-       -> add, recur
;; opponent -> add, return
;; my-side  -> don't add, return
(defn shift-walk [board pos shift]
    (loop [result #{}
           current-pos (shift-position pos shift)]
        (if (or (nil? current-pos)
                (not= (i/lookup board current-pos) \-))
            (filter not-nil? (conj result current-pos))
            (recur (conj result current-pos) 
                   (shift-position current-pos shift)))))

(defn shift-walks [board pos shifts]
    (mapcat #(shift-walk board pos %) shifts))

(defn my-pieces? [board & positions]
    (let [pieces (map #(i/lookup board %) positions)]
        (if (< (count positions) 2)
            (or (every? #(Character/isUpperCase %) pieces)
                (every? #(Character/isLowerCase %) pieces))
            nil)))

(defmulti get-valid-destinations (fn [board pos] (-> (i/lookup board pos)
                                                     Character/toLowerCase)))

(defmethod get-valid-destinations \r [board pos]
    (shift-walks board pos straight-shifts))

(defmethod get-valid-destinations \n [board pos]
    (filter not-nil?
            (map #(shift-position pos %) 
                 knight-shifts)))

(defmethod get-valid-destinations \b [board pos]
    (shift-walks board pos diagonal-shifts))

(defmethod get-valid-destinations \k [board pos]
    )

(defmethod get-valid-destinations \q [board pos]
    (shift-walks board pos (concat straight-shifts diagonal-shifts)))

(defmethod get-valid-destinations \p [board pos]
    )

(defmethod get-valid-destinations \- [board pos]
    (empty []))

(defmethod get-valid-destinations :default [board pos]
    (empty []))