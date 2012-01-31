(ns battleships.demo
  (:require clojure.set))

;; This is an example of how to create a player to submit to the server.
;; The entire namespace is submitted to the server.
;; You submit the file using the submit function in the client namespace.

(defn- random-coord
  "Generates a random valid coordinate."
  []
  (let [rows ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J"]
        columns (vec (range 1 11))]
    (str (rows (rand-int 10))
         (columns (rand-int 10)))))

(defn- random-orientation
  "Generates a random valid orientation."
  []
  ([:h :v] (rand-int 2)))

(defn place-ship
  "The ship is a map which represents the ship. You must return a map with the square you want to place it, and the orientation (:v is vertical, :h is horizontal)"
  [ship]
  {:square (random-coord) :orientation (random-orientation)})

(defn rowcol
  [p]
  (let [[row & col] p]
    [(str row) (Integer/parseInt (apply str col))]))

(defn next-val
  [v s]
  (-> (drop-while #(not (= % v)) s)
      rest
      first))

(defn prev-val
  [v s]
  (next-val v (reverse s)))

(defn next-row
  [r]
  (next-val r ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J"]))

(defn prev-row
  [r]
  (prev-val r ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J"]))

(defn next-col
  [c]
  (next-val c (vec (range 1 11))))

(defn prev-col
  [c]
  (prev-val c (vec (range 1 11))))

(defn adjacent-rows
  [r]
  (filter identity [(prev-row r) (next-row r)]))

(defn adjacent-cols
  [c]
  (filter identity [(prev-col c) (next-col c)]))

(defn adjacent
  [p]
  (let [[row col] (rowcol p)]
    (->> (concat (for [r [row] c (adjacent-cols col)] [r c])
                 (for [r (adjacent-rows row) c [col]] [r c]))
         (map #(apply str %)))))

(defn next-shot
  "Where do you want to attack next?"
  [context opponent-context]
  (let [adj (if (last (:hits context)) (set (adjacent (last (:hits context)))) #{})
        shots (set (:shots context))
        adj-free (vec (clojure.set/difference adj shots))
        ]
    (if (> (count adj-free) 0)
      (rand-nth adj-free)
      (random-coord))))
