(ns bst.core-spec
;  (:refer-clojure :exclude [])
  (:require [speclj.core :refer :all]
            [bst.core :refer :all])
  (:import [bst.core BST BNode])
  )

;; # The Tests
;;
;; We are going to use [spelj](https://github.com/slagyr/speclj) for our tests.

(describe "add"

          (it "should work on empty trees"
              (should= (BST. (BNode. nil 'a 5 nil) 1) (add (make-tree) 'a 5)))

          (it "should work on the same element and update"
              (let [x (BST. (BNode. nil 'a 5 nil) 1)]
                (should= (BST. (BNode. nil 'a 10 nil) 1) (add x 'a 10))))

          (it "should work multiple times"
              (let [x (BST. (BNode. (BNode. nil 'a 4 nil) 'b 5 (BNode. nil 'c 6 nil)) 3)]
                (should= (BST. (BNode. (BNode. nil 'a 4 nil) 'b 5 (BNode. nil 'c 6 (BNode. nil 'd 7 nil))) 4)
                         (add x 'd 7)))))


(describe "find"

          (it "should work"
              (let [x (BST. (BNode. (BNode. nil 'a 4 nil) 'b 5 (BNode. nil 'c 6 nil)) 3)]
                (should= 4 (find x 'a))))

          (it "checks right"
              (let [x (BST. (BNode. (BNode. nil 'a 4 nil) 'b 5 (BNode. nil 'c 6 nil)) 3)]
                (should= 6 (find x 'c)))))

(describe "find-key"

          (it "should work"
              (let [x (BST. (BNode. (BNode. nil 'a 4 nil) 'b 5 (BNode. nil 'c 6 nil)) 3)]
                (should= 'a (find-key x 4)))))

(describe "delete"

          (it "should work"
              (let [x (BST. (BNode. (BNode. nil 'a 4 nil) 'b 5 (BNode. nil 'c 6 nil)) 3)]
                (should= (BST. (BNode. (BNode. nil 'a 4 nil) 'b 5 nil) 2) (delete x 'c))))

          (it "should work with the top one"
              (let [x (BST. (BNode. (BNode. (BNode. nil 'a 1 nil) 'b 2 (BNode. nil 'c 3 nil)) 'd 4 (BNode. (BNode. nil 'e 5 nil) 'f 6 (BNode. nil 'g 7 nil))) 7)]
                (should= (BST. (BNode. (BNode. (BNode. nil 'a 1 nil) 'b 2 nil) 'c 3 (BNode. (BNode. nil 'e 5 nil) 'f 6 (BNode. nil 'g 7 nil))) 6) (delete x 'd))))

          (it "decements size by one"
              (let [x (BST. (BNode. (BNode. nil 'a 4 nil) 'b 5 (BNode. nil 'c 6 nil)) 3)]
                (should= 2 (size (delete x 'b)))))

          (it "does not make size less then 0"
              (let [x (make-tree)]
                (should= 0 (size (delete x 'b)))))

          (it "does not shorten subtrees"
              (let [x (BST. (BNode. (BNode. nil 'b 2 nil) 'd 4 (BNode. (BNode. nil 'e 5 nil) 'f 6 (BNode. nil 'g 7 nil))) 5)]
                (should= (BST. (BNode. (BNode. nil 'b 2 nil) 'd 4 (BNode. nil 'e 5 (BNode. nil 'g 7 nil))) 4) (delete x 'f))))

          (it "should work with the left side"
              (let [x (BST. (BNode. (BNode. (BNode. nil 'a 1 nil) 'b 2 (BNode. nil 'c 3 nil)) 'd 4 (BNode. (BNode. nil 'e 5 nil) 'f 6 (BNode. nil 'g 7 nil))) 7)]
                (should= (BST. (BNode. (BNode. nil 'a 1 (BNode. nil 'c 3 nil)) 'd 4 (BNode. (BNode. nil 'e 5 nil) 'f 6 (BNode. nil 'g 7 nil))) 6) (delete x 'b))))
          
           (it "another one"
              (let [x (BST. (BNode. (BNode. (BNode. nil 'a 1 nil) 'b 2 (BNode. nil 'c 3 nil)) 'd 4 (BNode. (BNode. nil 'e 5 nil) 'f 6 (BNode. nil 'g 7 nil))) 7)]
                (should= (BST. (BNode. (BNode. (BNode. nil 'a 1 nil) 'b 2 nil) 'd 4 (BNode. (BNode. nil 'e 5 nil) 'f 6 (BNode. nil 'g 7 nil))) 6) (delete x 'c)))))

(describe "delete-value"

          (it "should work"
              (let [x (BST. (BNode. (BNode. nil 'a 4 nil) 'b 5 (BNode. nil 'c 6 nil)) 3)]
                (should= (BST. (BNode. nil 'b 5 (BNode. nil 'c 6 nil)) 2) (delete-value x 4))))
          
          (it "should not make size less then 0"
              (let [x (make-tree)]
                (should= 0 (size (delete-value x 4)))))

          (it "should work on the right side"
              (let [x (BST. (BNode. (BNode. nil 'b 2 nil) 'd 4 (BNode. (BNode. nil 'e 5 nil) 'f 6 (BNode. nil 'g 7 nil))) 5)]
                (should= (BST. (BNode. (BNode. nil 'b 2 nil) 'd 4 (BNode. nil 'e 5 (BNode. nil 'g 7 nil))) 4) (delete-value x 6))))

          (it "should work with the top one"
              (let [x (BST. (BNode. (BNode. (BNode. nil 'a 1 nil) 'b 2 (BNode. nil 'c 3 nil)) 'd 4 (BNode. (BNode. nil 'e 5 nil) 'f 6 (BNode. nil 'g 7 nil))) 7)]
                (should= (BST. (BNode. (BNode. (BNode. nil 'a 1 nil) 'b 2 nil) 'c 3 (BNode. (BNode. nil 'e 5 nil) 'f 6 (BNode. nil 'g 7 nil))) 6) (delete x 'd))))

          (it "should decrement size by one"
              (let [x (BST. (BNode. (BNode. nil 'a 4 nil) 'b 5 (BNode. nil 'c 6 nil)) 3)]
                (should= 2 (size (delete-value x 4))))))

(describe "map-tree"

          (it "should work"
              (let [x (BST. (BNode. (BNode. nil 'a 4 nil) 'b 5 (BNode. nil 'c 6 nil)) 3)]
                (should= (BST. (BNode. (BNode. nil 'a 5 nil)  'b 6 (BNode. nil 'c 7 nil)) 3) (map-tree x inc)))))

(run-specs)
