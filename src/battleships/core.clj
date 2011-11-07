(ns battleships.core
  (:require [battleships.engine :as engine]
            [battleships.board :as board]
            [battleships.demo :as demo])
  (:import  [battleships.engine ShipPosition])
  (:gen-class :main true))


(defn make-random-cpu-player
  ([name]
     (reify engine/Player
       (get-name [this] name)
       (bot? [this] true)
       (ship-position [this ship]
         (let [pos (demo/place-ship ship)]
           pos))
       (next-shot [this {:keys [last-shot last-result hits misses ships-sunk] :as context}
                   opponent-context]
         (demo/next-shot context opponent-context))         
       (you-won [this {:keys [last-shot last-result hits misses ships-sunk]} opponent-context]
         (println (str name " " last-shot " = " last-result ", ships sunk = " ships-sunk))
         (println (str name " won in " (+ (count hits) (count misses)) " shots!")))
       (you-lost [this player-context opponent-context]
         (println (str name " lost!"))))))


(defn test-player [player]
  (engine/play player (make-random-cpu-player "cpu")))

(defn -main
  "Sets up the game"
  [& args]
  (engine/play (make-random-cpu-player "cpu1")
               (make-random-cpu-player "cpu2")))

