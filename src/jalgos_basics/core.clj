(ns jalgos-basics.core)

(defn mrg
  "Input: 3 collections (supposed to be used on already ordered collections for the first 2 ones)
   Output: 1 collections containing all elements of inputs in ascending order (if inputs are ordered)"
  [left right final]
  (loop [L left
         R right
         F final]
    (if (and (not-empty L) (not-empty R))
      (if (<= (first L) (first R))
        (recur (rest L) R (conj F (first L)))
        (recur (rest R) L (conj F (first R))))
      (concat final left right))))


(defn mrg-sort
  "Input: a collection (should work with a string as well)
   Output: vector of 2 collections approximately first half and second half of input"
  [coll]
  (if (> (count coll) 1)
    (let [sides (split-at (/ (count coll) 2) coll)]
      (mrg (mrg-sort (get sides 0)) (mrg-sort (get sides 1)) []))
    coll))
