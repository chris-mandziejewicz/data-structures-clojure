(ns traversals.core-spec
  (:require [speclj.core :refer :all]
            [traversals.core :refer :all])
  (:import [traversals.core BNode])
  )

;; # The Tests
;;
;; We are going to use [spelj](https://github.com/slagyr/speclj) for our tests.

;test
(describe "add"

          (it "should work on empty trees"
              (should= (BNode. nil 5 nil) (add nil 5)))

          (it "should work on the same element"
              (let [x (BNode. nil 5 nil)]
                (should= x (add x 5))))

          (it "should work several times"
              (let [x (BNode. (BNode. nil 3 nil) 4 nil)]
                (should= (BNode. (BNode. nil 3 nil) 4 (BNode. nil 5 nil))
                         (add x 5)))))

(describe "preorder"

          (it "should work"
              (let [x (reduce add nil '(4 2 3 5 9))]
                (should= '(4 2 3 5 9) (preorder x)))))

(describe "postorder"

          (it "should work"
              (let [x (reduce add nil '(4 2 3 5 9))]
                (should= '(3 2 9 5 4) (postorder x)))))

(describe "inorder"

          (it "should work"
              (let [x (reduce add nil '(4 2 3 5 9))]
                (should= '(2 3 4 5 9) (inorder x)))))

(describe "levelorder"

          (it "should work"
              (let [x (reduce add nil '(4 2 3 5 9))]
                (should= '(4 2 5 3 9) (levelorder x)))))

(describe "frontier"

          (it "should work"
              (let [x (reduce add nil '(4 2 3 5 9))]
                (should= '(3 9) (frontier x)))))

;; (describe "map-tree"

;;           (it "should work"
;;             (let [x (reduce add nil '(4 2 5))]
;;               (should= (BNode. (BNode. nil 3 nil) 5 (BNode. nil 6 nil))
;;                        (map-tree inc x)))))

(run-specs)