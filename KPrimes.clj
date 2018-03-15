;;https://www.codewars.com/kata/k-primes/train/clojure
(ns kprimes.core)

(defn fact [num]
  (loop [n num step 2 count 0]
    (cond 
      (<= n step) (inc count)
      (zero? (mod n step)) (recur (/ n step) 2 (inc count))
	  :else (recur n (inc step) count))))
   

(defn count-kprimes [k start nd]
  (filter #(= k (fact %)) (range start (inc nd)))) 
	
(defn puzzle-two [num step]
  (cond
    (<= num  step) 0  
    (and (= 3 (fact step)) (= 7 (fact (- num step)))) (inc (puzzle-two num (inc step)))
    :else (puzzle-two num (inc step))))
    
(defn puzzle-one [num step]
  (cond 
    (<= num step) 0
    (= 1 (fact step)) (+ (puzzle-two (- num step) 2) (puzzle-one num (inc step)))
    :else (puzzle-one num (inc step))))
    

(defn puzzle [num]
  (puzzle-one num 2))
