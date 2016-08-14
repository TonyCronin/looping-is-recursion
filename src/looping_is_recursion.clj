(ns looping-is-recursion)

;;; example recur example. Tail call opimisation
;;; recursion means the function uses it's return value
;;; to call the function again, thusly not generating
;;; an expression to be called at end.
(defn recur-factorial [number]
  (let [helper
        (fn [acc n] (if (zero? n)
                      acc
                      ;; here recur calls its containing fn again
                      (recur (* acc n) (dec n))))]
    (helper 1 number)))

;; (defn power [n k]
;;   (if (zero? k)
;;     1
;;     (* n (power n (dec k)))))

(defn power [base exp]
  (let [helper
        (fn [acc n]
          (if (zero? n)
            acc
            (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper
        (fn [last a-seq-2]
          (if (empty? a-seq-2)
            last
            (recur (first a-seq-2) (rest a-seq-2))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper
        (fn [b-seq1 b-seq2]
          (cond
            (not (= (first b-seq1) (first b-seq2))) false
            (and (empty? b-seq1) (empty? b-seq2)) true
            (not (= (empty? b-seq1) (empty? b-seq2))) false
            :else (recur (rest b-seq1) (rest b-seq2))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [seq-loop a-seq
         index      0]
    (cond
      (empty? seq-loop) nil
      (pred (first seq-loop)) index
      :else (recur (rest seq-loop) (inc index)))))

(defn avg [a-seq]
  (loop [index 0
         seq-loop a-seq
         total 0]
    (if (empty? seq-loop)
      (/ total index)
      (recur (inc index) (rest seq-loop) (+ total (first seq-loop))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (if (empty? a-seq)
    #{}
    (loop [a-set #{}
           xs a-seq]
      (if (empty? xs)
        a-set
        (recur (toggle a-set (first xs)) (rest xs))))))

(defn fast-fibo [n]
  (if (zero? n)
    0
    (loop [a 0
           b 1
           k 0
           acc 0]
      (if (= k n)
        acc
        (recur b acc (inc k) (+ acc b))))))

(defn cut-at-repetition [a-seq]
  (if (empty? a-seq)
    []
    (loop [xs a-seq
           ys #{}
           n 0]
      (cond
        (empty? xs) (apply vector (take n a-seq))
        (contains? ys (first xs)) (apply vector (take n a-seq))
        :else (recur (rest xs) (conj ys (first xs)) (inc n))))))
