;; The current challenge:
;; Let's create something that gives you a set of time-based datoms providing facts about the
;; state of the world, for example:
;; tN. is_raining
;; tN+1. is_not_raining
;; tN+n. sprinkler_on
;; tN+n2. sprinkler_off
;; etc
;; ala http://en.wikipedia.org/wiki/Bayesian_network

;; Then it automatically fills in the values of any not provided possible facts
;; for any new t, with a probability, first as if it did it in order, second as
;; if it knew the whole dataset in advance.

;; do we have to provide data like "was raining from time1 to time2" or can we just provide instants
;; and work it out...

;; if we work it out, would it have to be combination of time-closeness and bayes?  Well, we can
;; do bayes independent of time-closeness, then we can try to bring time-closeness into it and
;; see how we do...

(ns b3.core
  (:use [clj-time.core :only [date-time plus months after? interval millis in-millis]]))


(defn make-instant-fact
  "Constructs an 'instant-fact'."
  [instant fact]
  {:instant instant
   :fact fact})


(defn- one-in-n-chance
  [n]
  (fn [& _] (< (rand) (/ 1 n))))


(defn- no-sprinkler-in-rain [facts]
  (if (:is_raining facts)
    (filter #(not (= % :sprinkler_is_on)) facts)
    facts))


(defn rand-fact-samples
  "Generates n random facts from the distribution dist over a period of d days starting at start."
  [dist begin end n]
  {:pre [(pos? (count dist))
         (after? end begin)
         (>= n 0)]}
  (let [dist (vec dist)
        period (interval begin end)
        slice (millis (/ (in-millis period) n))]
    (loop [facts []
           begin begin
           n n]
      (if (<= n 0)
        facts
        (let [date (if (= n 1)     ;;  Ensures precision of final date.
                     end
                     begin)
              new-facts (set (map (partial make-instant-fact date)
                                  (filter (one-in-n-chance (count dist)) dist)))
              new-facts (no-sprinkler-in-rain new-facts)]
          (recur (vec (concat facts new-facts))
                 (plus begin slice)
                 (dec n)))))))


(def example-wiki-fact-set #{:is_raining
                             :sprinkler_is_on
                             :grass_is_wet})


(def example-wiki-fact-samples
  (let [begin (date-time 1975 7 22)
        end (plus begin (months 1))
        samples 30]
    (rand-fact-samples wiki-example-fact-set begin end samples)))
