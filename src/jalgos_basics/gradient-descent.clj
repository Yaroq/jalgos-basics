(ns jalgos-basics.gradient-descent
  (:use clojure.math.numeric-tower))

(defn grad-step
  "start with a one direction approx of gradient"
  [obj-f point step-size]
  (let [val-here (obj-f point)
        dim (count point)
        directions (for [i (range dim)] (flatten (list (repeat i 0) step-size (repeat (- dim (+ i 1)) 0))))
        steps (for [n (range dim)] (map + point (nth directions n)))
        step-there (map - (repeat dim val-here) (map obj-f steps))
        there (map + point step-there)
        val-there (obj-f there)]
    [there val-there]))


(defn grad-desc
  "Most naive implementation of gradient descent in clojure"
  [obj-f bounds & {:keys [nb-it step-size thresh] :or {nb-it 100000 step-size 0.01 thresh 0.0001}}]
  (let [low (take-nth 2 bounds)
        high (take-nth 2 (drop 1 bounds))
        center (map #(/ (+ %1 %2) 2) high low)]
  (loop [point center
         diff (+ thresh 1)
         iter nb-it]
    (if (and (< diff thresh) (> iter 0))
      {:minim point
       :value (obj-f point)}
      (recur (first (grad-step obj-f point step-size))
             (abs (- (obj-f point) (second (grad-step obj-f point step-size))))
             (dec iter))))))
