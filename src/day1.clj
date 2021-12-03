(ns day1
  (:require [clojure.string :as str]))


(defn read-report
  [report]
  (let [s (slurp report)
        xs (str/split s #"\n")]
    (map read-string xs)))

(def sample-report (read-report "resources/day1-sample-report.txt"))
(def report (read-report "resources/day1-report.txt"))

;; Part One

(defn inc-if-increasing
  [{:keys [prev cnt] :as acc} curr]
  (if (> curr prev)
    {:prev curr
     :cnt (+ cnt 1)}
    (assoc acc :prev curr)))


(defn count-increasing-depth-measurements
  [depths]
  (:cnt (reduce inc-if-increasing
                {:prev (first depths)
                 :cnt 0} 
                (rest depths))))

(count-increasing-depth-measurements sample-report) ; 7
(count-increasing-depth-measurements report) ; 1482

;; Part Two

(defn partition-and-reduce
  [depths]
  (let [triplets (partition 3 1 depths)]
    (map #(reduce + %) triplets)))

(count-increasing-depth-measurements (partition-and-reduce sample-report)) ; 5
(count-increasing-depth-measurements (partition-and-reduce report)) ; 1518


