(ns dlist-atom.core-spec
;  (:refer-clojure :exclude [reverse])
  (:require [speclj.core :refer :all]
            [dlist-atom.core :refer :all])
;  (:import [dlist-atom.core ])
  )

;; # The Tests
;;
;; We are going to use [spelj](https://github.com/slagyr/speclj) for our tests.


(describe "Insert-front"

          (it "should work with an empty list"
              (let [xx (dlist)]
                (insert-front xx 5)
                (should= '(5) (show-dlist xx))))
          
          (it "should work with a list with something in it"
              (let [xx (dlist)]
                (insert-front xx 5)
                (insert-front xx 10)
                (insert-front xx 15)
                (should= '(15 10 5) (show-dlist xx))))

          (it "should have size 1 on empty list"
              (let [xx (dlist)]
                (insert-front xx 5)
                (should= 1 (d-size xx))))

          (it "should update size when called multiple times"
              (let [xx (dlist)]
                (insert-front xx 5)
                (insert-front xx 10)
                (insert-front xx 15)
               (should= 3 (d-size xx))))

          (it "should work when called several times"
              (let [xx (dlist)]
                (insert-front xx 5)
                (insert-front xx 10)
                (insert-front xx 15)
                (should= '(15 10 5) (show-dlist xx))))

          (it "should not miss a back-link"
              (let [xx (dlist)]
                (insert-front xx 5)
                (insert-front xx 10)
                (should= 5 (-> xx d-sentinel d-prev d-data))))

          (it "should not miss a front-link"
              (let [xx (dlist)]
                (insert-front xx 5)
                (insert-front xx 10)
                (should= 10 (-> xx d-sentinel d-next d-data)))))

(describe "Insert-last"

          (it "should work with an empty list"
              (let [xx (dlist)]
                (insert-last xx 5)
                (should= '(5) (show-dlist xx))))

          (it "should have size 1 on empty list"
              (let [xx (dlist)]
                (insert-last xx 5)
                (should= 1 (d-size xx))))

          (it "should work when called several times"
              (let [xx (dlist)]
                (insert-last xx 5)
                (insert-last xx 10)
                (insert-last xx 15)
                (should= '(5 10 15) (show-dlist xx))))

          (it "(should update size when called multiple times"
              (let [xx (dlist)]
                (insert-last xx 5)
                (insert-last xx 10)
                (insert-last xx 15)
                (should= 3 (d-size xx)))))

(describe "Insert-sorted"

          (it "should work on empty list"
              (let [xx (dlist)]
                (insert-sorted xx 5)
                (should= '(5) (show-dlist xx))))

          (it "should have size 1 on empty list"
              (let [xx (dlist)]
                (insert-sorted xx 5)
                (should= 1 (d-size xx))))

          (it "should work when called several times"
              (let [xx (dlist)]
                (insert-sorted xx 30)
                (insert-sorted xx 10)
                (insert-sorted xx 20)
                (should= '(10 20 30) (show-dlist xx))))

          (it "should update size when called multiple times"
              (let [xx (dlist)]
                (insert-sorted xx 5)
                (insert-sorted xx 10)
                (insert-sorted xx 15)
                (should= 3 (d-size xx)))))

(describe "Index-forward"

          (it "should work properly"
              (should= 2 (index-forward (list-to-dlist '(1 2 3 4 5)) 3)))

          (it "is possible to return nil"
              (should= nil (index-forward (list-to-dlist '(1 2 3)) 100))))

(describe "Index-backward"

          (it "should work properly"
              (should= -3 (index-backward (list-to-dlist '(1 2 3 4 5)) 3))))

(describe "list-to-dlist"

          (it "should work on empty list"
              (let [xx '()
                    yy (list-to-dlist xx)]
                (should= '() (show-dlist yy))))

          (it "should work on full lists"
              (let [xx '(5 10 15)
                    yy (list-to-dlist xx)]
                (should= '(5 10 15) (show-dlist yy)))))

(describe "delete"

          (it "should do nothing to empty lists"
              (let [xx (dlist)]
                (delete xx 15)
                (should= '() (show-dlist xx))))

          (it "should work"
              (let [xx (dlist)]
                (insert-sorted xx 1)
                (insert-sorted xx 2)
                (insert-sorted xx 3)
                (insert-sorted xx 4)
                (insert-sorted xx 5)
                (delete xx 3)
                (should= '(1 2 4 5) (show-dlist xx)))))

(describe "reverse"

          (it "should work with empty lists"
              (let [xx (dlist)]
                (reverse xx)
                (should= '() (show-dlist xx))))

          (it "should work with full lists"
              (let [xx (dlist)]
                (insert-sorted xx 1)
                (insert-sorted xx 2)
                (insert-sorted xx 3)
                (insert-sorted xx 4)
                (insert-sorted xx 5)
                (reverse xx)
                (should= '(5 4 3 2 1) (show-dlist xx)))))

(describe "show-dlist-reverse"

          (it "should work with empty lists"
              (let [xx (dlist)
                   yy (show-dlist-reverse xx)]
                (should= '() yy)))

          (it "should work with full lists"
              (let [xx (list-to-dlist '(1 2 3 4 5))
                    yy (show-dlist-reverse xx)]
                (should= '(5 4 3 2 1) yy))))
(run-specs)