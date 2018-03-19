;;https://www.codewars.com/kata/going-to-zero-or-to-infinity
(defn going-origin [n]
  (loop [i 1 fct 1N rs 0N]
    (let 
      [fct-next (* fct i)]
      (if 
        (> i n)
        (* (/ 1 fct) rs) 
        (recur (inc i) fct-next (+ rs fct-next))))))
        
(defn going[n]
  (/ (int (* 1000000 (double (going-origin n)))) 1000000.0))
