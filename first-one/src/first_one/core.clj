(ns first_one.core)

(defn foo [x y]
  "A function to test if x > y."
  (> x y))

;; Take the absolute value.
(defn abs [x]
  (if (< x 0) (* x -1) x))

(defn bar [x y ]
  "A function to add x to y."
   (+ (abs x) (abs y)))
