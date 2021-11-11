;;;; tests/main.lisp
;; note: answers in this test suite relate to the data provided to me
;;       by the advent of code website. Different data sets are provided
;;       to different users - your data set and answers therefore will be
;;       different.

(defpackage #:advent2020/test
  (:use #:cl
        #:fiveam)
  (:export #:run!
           #:all-tests))

(in-package #:advent2020/test)

(def-suite advent2020
  :description "test suite - ensure refactoring doesn't break anything!")

(in-suite advent2020)

(test day1
  (is (= (day1:solution1) 440979))
  (is (= (day1:solution2) 82498112)))

(test day2
  (is (= (day2:solution1) 460))
  (is (= (day2:solution2) 251)))

(test day3
  (is (= (day3:solution1) 294))
  (is (= (day3:solution2) 5774564250)))

;; From day4, I set up the tests for the sample data as well. Once the solution
;; is successfully submitted, I create tests for the puzzle solution, to make
;; sure I don't break anything during the refactoring stage.

(test day4-test
  (is (= (day4:test1) 2))
  (is (= (day4:test2) 4)))

(test day4-solutions
  (is (= (day4:solution1) 237))
  (is (= (day4:solution2) 172)))

(test day5-test
  (is (= (day5:test1) 820)))

(test day5-solutions
  (is (= (day5:solution1) 806))
  (is (= (day5:solution2) 562)))

(test day6-test
  (is (= (day6:test1) 11)))

(test day6-solutions
  (is (= (day6:solution1) 6625))
  (is (= (day6:solution2) 3360)))

(test day7-part-a
  (is (= (day7:test1) 4))
  (is (= (day7:solution1) 248)))

(test day7-part-b
  (is (= (day7:test2) 32))
  (is (= (day7:solution2) 57281)))

(test day8-test-part-a
  (is (= (day8:test1) 5)))

(test day8-solution-part-a
  (is (= (day8:solution1) 1814)))

(test day8-test-part-b
  (is (= (day8:test2) 8)))

(test day8-solution-part-b
  (is (= (day8:solution2) 1056)))

(test day9-test
  (is (= (day9:test1) 127))
  (is (= (day9:test2) 62)))

(test day9-solutions
  (is (= (day9:solution1) 1038347917)))

(test day10-test1
  (is (= (day10:test1) 35))
  (is (= (day10:test2) 220)))

(test day10-test2
  (is (= (day10:test3) 8))
  (is (= (day10:test4) 19208)))

(test day10-solution1
  (is (= (day10:solution1) 1984))
  (is (= (day10:solution2) 3543369523456)))

(test day11-test1
  (is (= (day11:test1) 37)))

(test day11-solution1
  (is (= (day11:solution1) 2247)))

(test day11-test2
  (is (= (day11:test2) 26)))

(test day11-solution2
  (is (= (day11:solution2) 2011)))

(test day12-test1
  (is (= (day12:test1) 25)))

(test day12-solution1
  (is (= (day12:solution1) 759)))

(test day12-test2
  (is (= (day12:test2) 286)))

(test day12-solution2
  (is (= (day12:solution2) 45763)))

(test day13-test1
  (is (= (day13:test1) 295)))

(test day13-solution1
  (is (= (day13:solution1) 3464)))

(test day13-test2
  (is (= (day13:test2) 1068781)))

(test day13-solution2
  (is (= (day13:solution2) 760171380521445)))

(test day14-test1
  (is (= (day14:test1) 165)))

(test day14-solution1
  (is (= (day14:solution1) 5875750429995)))

(test day14-test2
  (is (= (day14:test2) 208)))

(test day14-solution2
  (is (= (day14:solution2) 5272149590143)))

(test day15-test1
  (is (= (day15:test1) 436)))

(test day15-solution1
  (is (= (day15:solution1) 1696)))

(test day15-test2
  (is (= (day15:test2) 175594)))

(test day15-solution2
  (is (= (day15:solution2) 37385)))

(test day16-test1
  (is (= (day16:test1) 71)))

(test day16-solution1
  (is (= (day16:solution1) 22073)))

(test day16-solution2
  (is (= (day16:solution2) 1346570764607)))

(test day17-test1
  (is (= (day17:test1) 112)))

(test day17-solution1
  (is (= (day17:solution1) 401)))

(test day17-test2
  (is (= (day17:test2) 848)))

(test day17-solution2
  (is (= (day17:solution2) 2224)))

(test day18-test1
  (is (= (day18:test1) 26))
  (is (= (day18:test2) 437))
  (is (= (day18:test3) 12240))
  (is (= (day18:test4) 13632)))

(test day18-solution1
  (is (= (day18:solution1) 3647606140187)))

(test day18-solution2
  (is (= (day18:solution2) 323802071857594)))

(test day19-test1
  (is (= (day19:test1) 2)))

(test day19-solution1
  (is (= (day19:solution1) 151)))

(test day19-test2
  (is (= (day19:test2) 12)))

(test day19-solution2
  (is (= (day19:solution2) 386)))

(test day20-test1
  (is (= (day20:test1) 20899048083289)))

