(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq)
         (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (= false (pred? (first a-seq))) a-seq
    :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not= (first a-seq) (first b-seq)) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-2) (empty? seq-1)) ()
    () (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    :else ()))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    ()
    (cons what-to-repeat (lazy-seq (my-repeat (dec how-many-times) what-to-repeat)))))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (- up-to 1) (lazy-seq (my-range (dec up-to))))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list ())
    (cons a-seq (lazy-seq (tails (rest a-seq))))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (list ())
    (cons a-seq (lazy-seq (inits (drop-last a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list ())
    (take (count a-seq) (partition (count a-seq) 1 (cycle a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [counted (if (contains? freqs (first a-seq))
                    (assoc freqs (first a-seq) (inc (freqs (first a-seq))))
                    (assoc freqs (first a-seq) 1))]
      (my-frequencies-helper counted (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    a-map
    (concat (repeat ((first a-map) 1) ((first a-map) 0))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (= 1 n) (empty? coll))
    (rest coll)
    (my-drop (dec n) (rest coll))))

(defn halve-helper [a-seq counter]
  (if (zero? counter)
    ()
    (cons (first a-seq) (halve-helper (rest a-seq) (dec counter)))))

(defn halve [a-seq]
  (conj [] (halve-helper a-seq (int (/ (count a-seq) 2)))
        (drop (int (/ (count a-seq) 2)) a-seq)))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (if (>= (first a-seq) (first b-seq))
            (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
            (cons (first a-seq) (seq-merge (rest a-seq) b-seq)))))

(defn merge-sort [a-seq]
  (if (> 2 (count a-seq))
    a-seq
    (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
