(ns day2
  (:require [clojure.string :as str]))


(defn read-report
  [report]
  (let [s (slurp report)
        xs (str/split s #"\n")]
    (map (fn [s] 
           (let [[direction distance] (str/split s #" ")] 
             [direction (read-string distance)]))
                 xs)))

(def sample-report (read-report "resources/day2-sample-report.txt"))
(def report (read-report "resources/day2-report.txt"))

(def up "up")
(def down "down")
(def forward "forward")

(defn get-depth-and-distance
  [movements]
  (reduce (fn [{:keys [hor dep] :as loc} [dir dis]]
            (condp = dir
              up (assoc loc :dep (- dep dis))
              down (assoc loc :dep (+ dep dis))
              forward (assoc loc :hor (+ hor dis))))
          {:dep 0 :hor 0}
          movements))

(let [{:keys [hor dep]} (get-depth-and-distance sample-report)]
  (* hor dep)) ; 150

(let [{:keys [hor dep]} (get-depth-and-distance report)]
  (* hor dep)) ; 2322630

(defn get-depth-and-distance-with-aim
  [movements]
  (reduce (fn [{:keys [hor dep aim] :as loc} [dir dis]]
            (condp = dir
              up (assoc loc :aim (- aim dis))
              down (assoc loc :aim (+ aim dis))
              forward (assoc loc 
                             :hor (+ hor dis)
                             :dep (+ dep (* aim dis)))))
          {:dep 0 :hor 0 :aim 0}
          movements))

(let [{:keys [hor dep]} (get-depth-and-distance-with-aim sample-report)]
  (* hor dep)) ; 900

(let [{:keys [hor dep]} (get-depth-and-distance-with-aim report)]
  (* hor dep)) ; 2105273490
