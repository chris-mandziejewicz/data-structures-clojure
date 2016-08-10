(ns fifolifo.core-spec
  (:refer-clojure :exclude [pop peek])
  (:require [speclj.core :refer :all]
            [fifolifo.core :refer :all])
  (:import [fifolifo.core Stack Queue]))

;; # The Tests
;;
;; We are going to use [spelj](https://github.com/slagyr/speclj) for our tests.


(describe "The stack declaration"

          (it "should create something."
              (should (make-stack)))

          (it "should have empty components."
              (should= (Stack. nil 0) (make-stack)))

          (it "should have a size of zero."
              (should= 0 (stack-size (make-stack))))
          )


(describe "The queue declaration"

          (it "should create something."
              (should (make-queue)))

          (it "should have empty components."
              (should= (Queue. nil nil 0) (make-queue)))

          (it "should have a size of zero."
              (should= 0 (stack-size (make-stack))))
          )

(describe "stack size"

          (it "should return 0 if the stack has empty components."
              (should= 0 (stack-size (make-stack))))

          (it "should return the size if there are components."
              (should= 3 (stack-size (Stack. '(1 2 3) 3))))
          )

(describe "push"

          (it "should add the element to the top if it is empty"
              (should= (Stack. '(1) 1) (push (make-stack) 1)))

          (it "should add the element to the top even it is not empty"
              (should= (Stack. '(1 2 3) 3) (push (push (push (make-stack) 3) 2) 1)))
          )

(describe "pop"

          (it "should work with an empty stack"
              (should= (Stack. nil 0) (pop (make-stack))))

          (it "should work with non-empty stacks"
              (should= (Stack. '(1 2 3) 3) (pop (Stack. '(5 1 2 3) 4))))
          )

(describe "top"

          (it "should return the top of the stack"
              (should= 4 (top (Stack. '(4 5 6) 3))))
          )

(describe "queue-size"

          (it "should return the size of the queue"
              (should= 5 (queue-size (Queue. '(1 2) '(3 4 5) 5))))
          )

(describe "enqueue"

           (it "should work with an empty queue"
               (should= (Queue. '(1) nil 1) (enqueue (make-queue) 1)))

          (it "should work with a non-empty queue"
              (should= (Queue. '(1 2 3) nil 3) (enqueue (enqueue (enqueue (make-queue) 3) 2) 1)))
          )

(describe "dequeue"

          (it "should work with an empty queue"
              (should= (Queue. nil nil 0) (dequeue (make-queue))))

          (it "should work with several elements in back and none in front"
              (should= (Queue. nil '(3 2 1) 3) (dequeue (Queue. '(1 2 3 4) nil 4))))

          (it "should work with elements in the back and front"
              (should= (Queue. '(5) '(4 3 2) 4) (dequeue (Queue. '(5) '(5 4 3 2) 5))))

          (it "should work with several elements in the front and non in the back"
              (should= (Queue. nil '(4 3 2 1) 4) (dequeue (Queue. nil '(5 4 3 2 1) 5))))
          )

(describe "peek"

          (it "should return the next element to come out of front."
              (should= 10 (peek (Queue. '(1 2 3) '(10 11 12) 6))))
          )
(run-specs)