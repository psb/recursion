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
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (let [[fst snd & rest] a-seq]
            (max-element (cons (max fst snd)
                               rest)))))

(defn seq-max [seq-1 seq-2]
  (last (sort [seq-1 seq-2])))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (let [[fst snd & rest] a-seq]
            (longest-sequence (cons (seq-max fst snd)
                                    rest)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (cons (first a-seq)
                                (my-take-while pred? (rest a-seq)))
    :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not= (count a-seq) (count b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (zero? n) n
    (zero? k) 1
    :else (* n (power n (dec k)))))

(defn fib [n]
  (case n
    0 0
    1 1
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons '() a-seq)
    (cons (vec a-seq)
          (tails (vec (rest a-seq))))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons '() a-seq)
    (cons (vec a-seq)
          (inits (vec (butlast a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons '() a-seq)
    (map-indexed (fn [idx itm] (concat (drop idx a-seq)
                                       (take idx a-seq)))
                 a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          new-freqs (if (contains? freqs elem)
                      (assoc freqs elem (inc (get freqs elem)))
                      (assoc freqs elem 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    a-map
    (concat (let [[k v] (first a-map)]
              (repeat v k))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (cond
    (empty? coll) '()
    (zero? n) '()
    (> n (count coll)) coll
    :else (concat (vector (first coll))
                  (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) '()
    (zero? n) '()
    (> n (count coll)) '()
    :else (reverse (concat (vector (last coll))
                           (my-drop (dec n) (butlast coll))))))

(defn halve [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [half (int (/ (count a-seq) 2))]
      (vector (take half a-seq)
              (drop half a-seq)))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (let [fst-a (first a-seq)
                fst-b (first b-seq)]
            (if (< fst-a fst-b)
              (cons fst-a (seq-merge (rest a-seq) b-seq))
              (cons fst-b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (or (nil? (first a-seq))
          (nil? (next a-seq)))
    a-seq
    (let [[fst-half snd-half] (halve a-seq)]
      (seq-merge (merge-sort fst-half) (merge-sort snd-half)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

