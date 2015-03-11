(ns jalgos-basics.stoch)

"RANDOM SEARCH
ultra simple implementation of basic optimization algorithm random search"

(defn uni-sample
  [bounds]
"I don't test relevancy of inputs"
  (let [low (take-nth 2 bounds)
        high (take-nth 2 (drop 1 bounds))
        sizes (map - high low)]
    (map #(+ (rand %1) %2) sizes low)))

(defn random-val
  ([obj-f bounds nb-it]
   (let [x (uni-sample bounds)
         y (obj-f x)]
     (random-val obj-f bounds (dec nb-it) [x y])))
  ([obj-f bounds nb-it benchmark]
   (let [x (uni-sample bounds)
         y (obj-f x)]
     (if (= 0 nb-it)
       benchmark
       (if (< y (last benchmark))
         (random-val obj-f bounds (dec nb-it) [x y])
         (random-val obj-f bounds (dec nb-it) benchmark))))))

