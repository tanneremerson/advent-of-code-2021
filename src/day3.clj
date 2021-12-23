(ns day3
  (:require [clojure.string :as str]))


(defn read-report
  [report]
  (let [s (slurp report)
       xs (str/split s #"\n")]
    (map #(->> % seq) xs)))

(defn convert-chars-to-strs
  [v]
  (map #(map (fn [value] 
               (if (= value \0)
                 "0"
                 "1")) 
             %) v))

(def sample-report (convert-chars-to-strs (read-report "resources/day3-sample-report.txt")))
(def report (convert-chars-to-strs (read-report "resources/day3-report.txt")))


(defn parse-binary-str
  [s]
  (Integer/parseInt s 2))

(defn seq->binary-str
  [s]
  (apply str s))

(defn get-power-consumption
  [diagnostics]
  (let [freq (map frequencies
                  (apply map list diagnostics))
        f (fn [c g] 
            (if (g (get c "1") (get c "0")) "1" "0"))
        s1 (map #(f % >) freq)
        s2 (map #(f % <) freq)
        gamma (parse-binary-str (apply str s1))
        epsilon (parse-binary-str (apply str s2))]
    (* gamma epsilon)))

(get-power-consumption sample-report) ; 198
(get-power-consumption report) ; 4139586


(defn count-filter-recur
  [values pos pred]
  (if (= 1 (count values))
    (first values)
    (let [{:keys [zeros ones] :as _cnts} 
          (reduce #(if (= (nth %2 pos) "0")
                     (assoc %1 :zeros (inc (:zeros %1)))
                     (assoc %1 :ones (inc (:ones %1))))
                  {:zeros 0 :ones 0}
                  values)
          _ (prn "pos: " pos ", z: " zeros " , o: " ones)

          keep-value (if (pred zeros ones) "0" "1")
          updated-values (filter #(= (nth % pos) keep-value) values) ]
      (recur updated-values (inc pos) pred))))


(defn get-support-rating
  [values]
  (let [oxygen-rating (-> values
                          (count-filter-recur 0 >)
                          seq->binary-str
                          parse-binary-str)
        co2-rating (-> values
                       (count-filter-recur 0 <=)
                       seq->binary-str
                       parse-binary-str)]
    (* oxygen-rating co2-rating)))

(get-support-rating sample-report)
(get-support-rating report)

