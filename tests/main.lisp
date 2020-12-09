;;;; tests/main.lisp
;; note: answers in this test suite relate to the data provided to me
;;       by the advent of code website. Different data sets are provided
;;       to different users - your data set and answers therefore will be
;;       different.

(defpackage #:advent2020/test
  (:use #:cl
        #:advent2020
        #:fiveam)
  (:export #:run!
           #:all-tests))

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

(test day4-test
  (is (= (day4/test1) 2))
  (is (= (day4/test2) 4)))

(test day4-solutions
  (is (= (day4/solution1) 237))
  (is (= (day4/solution2) 172)))

(test day5-test
  (is (= (day5/test1) 820)))

(test day5-solutions
  (is (= (day5/solution1) 806))
  (is (= (day5/solution2) 562)))

(test day6-test
  (is (= (day6/test1) 11)))

(test day6-solutions
  (is (= (day6/solution1) 6625))
  (is (= (day6/solution2) 3360)))

(test day7-part-a
  (is (= (day7/test1) 4))
  (is (= (day7/solution1) 248)))

(test day7-part-b
  (is (= (day7/test2) 32))
  (is (= (day7/solution2) 57281)))

(test day8-test-part-a
  (is (= (day8/test1) 5)))

(test day8-solution-part-a
  (is (= (day8/solution1) 1814)))

(test day8-test-part-b
  (is (= (day8/test2) 8)))

(test day8-solution-part-b
  (is (= (day8/solution2) 1056)))

(test day9-test
  (is (= (day9/test1) 127)))

