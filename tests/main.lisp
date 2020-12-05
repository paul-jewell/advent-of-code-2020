;;;; tests/main.lisp
;; note: answers in this test suite relate to the data provided to me
;;       by the advent of code website. Different data sets are provided
;;       to different users - your data set and answers therefore will be
;;       different.

(in-package #:advent2020/test)

(def-suite advent2020
  :description "test suite - ensure refactoring doesn't break anything!")

(in-suite advent2020)

(test day1
  (is (= (day1/solution1) 440979))
  (is (= (day1/solution2) 82498112)))

(test day2
  (is (= (day2/solution1) 460))
  (is (= (day2/solution2) 251)))

(test day3
  (is (= (day3/solution1) 294))
  (is (= (day3/solution2) 5774564250)))

;; From day4, I set up the tests for the sample data as well. Once the solution
;; is successfully submitted, I create tests for the puzzle solution, to make
;; sure I don't break anything during the refactoring stage.

(test day4
  (is (= (day4/test1) 2))
  (is (= (day4/solution1 237))))


(defun test-advent2020 ()
  (run! 'advent2020))
