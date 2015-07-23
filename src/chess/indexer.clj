(ns chess.indexer)

(def file-key \a)

(def rank-key \0)

(defn- file-component [file]
    (- (int file) 
       (int file-key)))

(defn- rank-component [rank]
    (->> (- (int rank)
            (int rank-key))
	     (- 8)
	     (* 8)))

(defn index 
    ([[file rank]] (index file rank))
    ([file rank] (+ (file-component file) (rank-component rank))))

(defn lookup [board pos]
    (let [[file rank] pos]
        (board (index file rank))))

(defn- define-board [start]
    (let [end (+ start 8)]
        (map char (range start end))))

(defn valid-files []
    (define-board (int file-key)))

(defn valid-ranks []
    (define-board (inc (int rank-key))))