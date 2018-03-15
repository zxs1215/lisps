;;codewars kata 
;;https://www.codewars.com/kata/second-variation-on-caesar-cipher/train/clojure
(ns caesar2.core)
(defn circle-ascii [intch min max]
  (let [length (inc (- max min))]
    (cond  
      (> intch max) (circle-ascii  (- intch length) min max)
      (< intch min) (circle-ascii  (+ intch length) min max)
      :else (char intch))))
		   
(defn shift-char [ch shift]
  (cond 
    (Character/isLowerCase ch) (circle-ascii (+ (int ch) shift) (int \a) (int \z)) 
    (Character/isUpperCase ch) (circle-ascii (+ (int ch) shift) (int \A) (int \Z))
    :else ch))

(defn upper-char [ch]
  (if (Character/isLowerCase ch)
    (char (+ (int ch) (- (int \A) (int \a))))
    ch))
  
(defn lower-char [ch]
  (if (Character/isUpperCase ch)
    (char (+ (int ch) (- (int \a) (int \A))))
    ch))
	
(defn get-rotate [shift ch]
  (str ch (shift-char ch shift)))
  
(defn encode-normal [shift ch]
  (str (shift-char ch shift)))
	

(defn encode-first [ch shift]
  (str (get-rotate shift (lower-char ch)) (shift-char ch shift)))
	
(defn encode [s shift]
  (apply str 
    (concat 
      (encode-first (first s) shift) 
      (map (partial encode-normal shift) (rest s)))))
  
(defn avg-size [count]
  (do (println count)
    (if (zero? (mod count 5))
      (/ count 5)
      (inc (int (/ count 5))))))

(defn encode-str [s shift]
  (let
    [rs (encode s shift)
    size (avg-size (count rs))]
    (do (println s)
      (map 
        #(apply str %) 
	(partition-all size rs)))))
	  
  
(defn decode-first [s shift] 
  (subs s 2))

(defn decode-lower-char [shift ch]
  (shift-char ch (- shift)))
  
(defn decode-lower [s shift]
  (apply str (map (partial decode-lower-char shift)s)))

(defn decode-str [s shift]
  (decode-lower (decode-first s shift) shift))  
  
(defn mydecode [s shift] 
  (decode-str (clojure.string/join s) shift))

(defn decode [s] (mydecode s 1))



  
