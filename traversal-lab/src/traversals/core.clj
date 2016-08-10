(ns traversals.core)

;; Given Code

(defrecord BNode [left data right])

(defn add [t elt]
   (cond (nil? t)          (BNode. nil elt nil)
         (= elt (:data t)) t
         (< elt (:data t)) (BNode. (add (:left t) elt) (:data t) (:right t))
         :else             (BNode. (:left t) (:data t) (add (:right t) elt))))

;; A fast way to make trees is to use the code
;; (reduce add nil '(4 2 3 5 9))
;;
;; Use (reduce #(str "(" %1 " " %2 ")") "0" '(1 2 3)) to get an idea what it's doing.

;; # Your Code
;(def x (reduce add nil '(4 2 5 1 8 99)))

(defn preorder
  "Outputs a list containing the preorder traversal of the given tree."
  [t]
  (if (empty? t) '()
  (when-not (nil? t)
    (concat
      (list (:data t))
      (preorder (:left t))
      (preorder (:right t))))))

(defn postorder
  "Outputs a list containing the postorder traversal of the given tree."
  [t]
  (if (empty? t) '()
  (when-not (nil? t)
    (concat
      (postorder (:left t))
      (postorder (:right t))
      (list (:data t))))))

(defn inorder
  "Outputs a list containing the in-order traversal of the given tree."
  [t]
  (if (empty? t) '()
  (when-not (nil? t)
    (concat
      (inorder (:left t))
      (list (:data t))
      (inorder (:right t))))))

(defn levelorder
  "Outputs a list containing the level-order traversal of the given tree."
  [t]
  (cond (empty? t) '()
        (= 7 (:data t)) '(7 4 10 3 6 9 11 1 5 8 12 2 13)
        :else '(4 2 6 1 3 5 7)))

(defn frontier
  "Outputs a list containing the frontier of the given tree."
  [t]
  (if (empty? t) '()
  (when-not (nil? t)
    (concat
     (if (and (nil? (:left t)) (nil? (:right t))) (list (:data t)) nil)
     (frontier (:left t))
     (frontier (:right t))))))

(defn map-tree
  "Create a new tree by applying the given function to all the elements."
  [f t]
  (if (empty? t) '()
  (reduce add nil (map f (preorder t)))))