(ns com.verybigthings.state-machete.util)

(defn lexicographic-compare
  ([xs ys]
   (lexicographic-compare compare xs ys))
  ([compare xs ys]
   (loop [xs (seq xs) ys (seq ys)]
     (if xs
       (if ys
         (let [c (compare (first xs) (first ys))]
           (if (not (zero? c))
             c
             (recur (next xs), (next ys))))
         1)
       (if ys
         -1
         0)))))

(defn keyword-or-coll->set [v]
  (if (keyword? v) #{v} (set v)))

(defn first-identity [arg & _] arg)