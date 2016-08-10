(ns bst.core)

;; # Introduction
;;
;; In this lab you get to write a BST like the one we did in class, only
;; this time it is a dictionary structure and not a set.
;; As such, the "data" element from before will have a key and value instead.

(defrecord BST [root size])
(defrecord BNode [left key value right])

(defn make-node
  ([key value]  (make-node nil key value nil))
  ([left key value right] (BNode. left key value right))
  )

(defn make-tree []
  (BST. nil 0))

;; # Size
;;
;; A warmup function.

(defn size "Return the size of the tree."
  [t]
  (:size t))

;; # Add
;;
;; The nodes will be entered into the tree on the basis of their key.
;; If someone tries to add a key that is already there, we replace the value
;; with the new entry.


(defn add-helper [node nu-key nu-val]
  (cond (nil? node) (make-node nil nu-key nu-val nil)
        (= nu-key (:key node)) (make-node (:left node) nu-key nu-val (:right node))
        (neg? (compare nu-key (:key node))) (make-node (add-helper (:left node) nu-key nu-val) (:key node) (:value node) (:right node))
        :else (make-node (:left node) (:key node) (:value node) (add-helper (:right node) nu-key nu-val))))

(defn size-helper [node nu-key nu-val]
  (cond (nil? node) true
        (= nu-key (:key node)) false
        (neg? (compare nu-key (:key node))) (size-helper (:left node) nu-key nu-val)
        :else (size-helper (:right node) nu-key nu-val)))

(defn add "Add a key and value to the BST."
  [bst nu-key nu-val]
  (BST. (add-helper (:root bst) nu-key nu-val)
        (cond (= 0 (size bst)) (inc (size bst))
              (true? (size-helper (:root bst) nu-key nu-val)) (inc (size bst))
              :else (size bst))))

;(def x (make-tree))
;(def x (add x 'd 10))
;(def x (add x 'b 11))
;(def x (add x 'f 12))
;(def x (add x 'a 13))
;(def x (add x 'c 14))
;(def x (add x 'e 15))
;(def x (add x 'g 16))
;x

;; # Find
;;
;; We need two versions of find.  The first one takes a key and returns the
;; value.  The second takes a value and returns the key.  Note that the second
;; version of the function must search the entire tree!  If the search item is not
;; there, return nil.

(defn help-find
  [node key]
  (cond (nil? node) nil
        (= key (:key node)) (:value node)
        (neg? (compare key (:key node))) (help-find (:left node) key)
        :else (help-find (:right node) key)))

(defn find "Look for a key and return the corresponding value."
  [bst look-key]
  (help-find (:root bst) look-key))

(defn help-find-key
  [node]
  (if (empty? node) nil
    (when-not (nil? node)
      (concat
       (list (:key node))
       (list (:value node))
       (help-find-key (:left node))
       (help-find-key (:right node))))))

(defn look-list
  [lst value]
  (cond (empty? lst) nil
        (= value (second lst)) (first lst)
        :else (look-list (rest (rest lst)) value)))

(defn find-key "Look for a value and return the corresponding key."
  [bst look-value]
  (look-list (help-find-key (:root bst)) look-value))

;; # Delete
;;
;; Similiarly, we have two versions of delete.  Please use the predecessor node if
;; you need to delete a child with two elements.

(defn go-right [t]
  (if (nil? (:right t)) t
    (go-right (:right t))))

(defn get-pred [node]
                (go-right (:left node)))

(defn delete-helper
  [node victim]
  (cond (nil? node) nil
        (= victim (:key node))
          (cond (and (nil? (:left node)) (nil? (:right node))) nil
                (or (nil? (:left node)) (nil? (:right node)))
                  (or (:left node) (:right node))
                :else (let [pred (get-pred node)]
                        (make-node (delete-helper (:left node) (:key pred))
                                   (:key pred)
                                   (:value pred)
                                   (:right node)))
                )
        (neg? (compare victim (:key node)))
          (make-node (delete-helper (:left node) victim) (:key node) (:value node) (:right node))
        :else
          (make-node (:left node) (:key node) (:value node) (delete-helper (:right node) victim))
        ))
        
(defn delete [bst victim]
 (BST. (delete-helper (:root bst) victim) (if (= (:root bst) (delete-helper (:root bst) victim)) (size bst)
                                            (dec (size bst)))))

(defn delete-value [bst victim]
  (delete bst (find-key bst victim)))

;; # Map Tree
;;
;; This function takes a tree t and maps a function f over it.
;; If your tree is ((x 3 x) 5 ((x 7 x) 6 x)), then (map-tree t inc)
;; will return ((x 4 x) 6 ((x 8 x) 7 x))

(defn preorder
  "Outputs a list containing the preorder traversal of the given tree."
  [t]
  (if (empty? t) '()
  (when-not (nil? t)
    (concat
      (list (:key t))
      (list (:value t))
      (preorder (:left t))
      (preorder (:right t))))))

 (defn map-helper [t f]
   (if (empty? t) nil
     (cons (if (symbol? (first t)) (read-string (str (first t)))
               (read-string (str (first (list (f (first t))))))) 
           (map-helper (rest t) f))))
 
 (defn tree-helper [init lst]
   (if (empty? lst) init
   (tree-helper (add init (first lst) (second lst)) (rest (rest lst)))))
 
(defn to-tree [t]
  (empty? t) nil
  (tree-helper (add (make-tree) (first t) (second t)) (rest (rest t))))

(defn map-tree
  "Create a new tree by applying the given function to all the elements."
  [t f]
  (to-tree (map-helper (preorder (:root t)) f)))