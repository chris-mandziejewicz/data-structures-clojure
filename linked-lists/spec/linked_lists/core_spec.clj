(ns linked_lists.core-spec
  (:require [speclj.core :refer :all]
            [linked_lists.core :refer :all])
  (:import [linked_lists.core Cons]))

;; # The Tests
;;
;; We are going to use [spelj](https://github.com/slagyr/speclj) for our tests.


(describe "The record declaration"
          (it "should create something"
              (should (Cons. 10 20)))

          (it "should have a car"
              (should= 10 (:car (Cons. 10 20))))

          (it "should have a cdr"
              (should= 20 (:cdr (Cons. 10 20))))

          (it "should be chainable"
              (should= 40 (-> (Cons. 10 (Cons. 20 (Cons. 30 40))) :cdr :cdr :cdr))))

(describe "insert-at-beginning"
          (it "creates a cons cell"
              (should-not= nil (insert-at-beginning 10 nil)))

          (it "should work with empty lists"
              (should= (Cons. 10 nil) (insert-at-beginning 10 nil) ))
          
          (it "should work with lists that have data"
              (let [xx (Cons. 10 (Cons. 20 (Cons. 30 nil)))]
                (should= (Cons. 5 xx) (insert-at-beginning 5 xx) ))))

(describe "insert-at-end"
          (it "creates a cons cell"
              (should= (Cons. 20 nil) (insert-at-end 20 nil)))
          (it "should work with empty lists"
              (should-not= nil (insert-at-end 55 nil)))
          (it "should insert at the far end, copying the whole list as it goes"
              (let [xx (Cons. 20 (Cons. 30 nil))]
                (should= (Cons. 20 (Cons. 30 (Cons. 25 nil))) (insert-at-end 25 xx)))))

(describe "sorted insert"
          (it "should work with empty lists"
              (should-not= nil (sorted-insert 25 nil)))
          (it "should recycle memory"
              (let [xx (Cons. 20 (Cons. 30 nil))]
                (should (identical? (:cdr xx) (:cdr (:cdr (sorted-insert 25 xx)))))))
          (it "should insert an element into a sorted list"
              (let [xx (Cons. 20 (Cons. 25 (Cons. 30 nil)))]
                (should= (Cons. 20 (Cons. 25 (Cons. 27 (Cons. 30 nil)))) (sorted-insert 27 xx)))))

(describe "search"
          (it "should work with empty lists"
              (should= false (search 5 nil)))
          (it "should be able to find the element if it is in the list"
              (let [xx (Cons. 20 (Cons. 25 (Cons. 30 nil)))]
                (should= true (search 25 xx))))
          (it "should return false if the element is not in the list"
              (let [xx (Cons. 20 (Cons. 25 (Cons. 30 nil)))]
                (should= false (search 35 xx)))))

(describe "delete"
          (it "should return nil if the list is empty"
              (should= nil (delete 5 nil)))
          (it "Should return the list if there is no element to delete"
              (let [xx (Cons. 20 (Cons. 25 (Cons. 30 nil)))]
                (should= xx (delete 35 xx))))
          (it "Should not delete all the elements"
              (let [xx (Cons. 20 (Cons. 20 (Cons. 20 (Cons. 30 nil))))]
                (should-not= (Cons. 30 nil) (delete 20 xx))))
          (it "Should delete the element if it is in the list"
              (let [xx (Cons. 20 (Cons. 25 (Cons. 30 nil)))]
                (should= (Cons. 20 (Cons. 30 nil)) (delete 25 xx)))))

(describe "delete-all"
          (it "should return nil if the list is empty"
              (should= nil (delete-all 5 nil)))
          (it "should return the list if the element is not in the list"
              (let [xx (Cons. 20 (Cons. 25 (Cons. 30 nil)))]
                (should= xx (delete-all 5 xx))))
          (it "should delete all the elements found in the list"
              (let [xx (Cons. 4 (Cons. 5 (Cons. 5 (Cons. 5 (Cons. 6 (Cons. 5 (Cons. 7 nil)))))))]
                (should= (Cons. 4 (Cons. 6 (Cons. 7 nil))) (delete-all 5 xx)))))

(describe "efficient-delete"
          (it "should return nil if the list is empty"
              (should= nil (efficient-delete 5 nil)))
          (it "should delete the element if it is in the list"
              (let [xx (Cons. 20 (Cons. 25 (Cons. 30 nil)))]
                (should= (Cons. 20 (Cons. 30 nil)) (efficient-delete 25 xx))))
          (it "should return the original list if the element is not in the list"
              (let [xx (Cons. 20 (Cons. 25 (Cons. 30 nil)))]
                (should (identical? xx (efficient-delete 35 xx))))))

(run-specs)
