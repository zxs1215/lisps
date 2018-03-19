;;https://www.codewars.com/kata/pyramid-slide-down
(defn max-path [dp line index v]
  (let 
    [left (get (get dp (dec line)) (dec index))
     right (get (get dp (dec line)) index)]
    (max 
      (if (nil? left) v (+ left v))
      (if (nil? right) v (+ right v)))))
    
(defn next-dp [dp pyramid i]
  (apply vector
    (map-indexed   
      #(max-path dp i %1 %2)
      (pyramid i))))      
      
(defn longest-slide-down [pyramid]
  (let [length (count pyramid)]
    (loop [dp [] i 0]
      (cond 
        (zero? i) (recur (conj dp (pyramid 0)) (inc i))
        (= i length)  (do (println dp) (apply max (dp (dec i))))
        (< i length)  (recur (conj dp (next-dp dp pyramid i)) (inc i))     
        :else nil)))) 
