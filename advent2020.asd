;;;; advent2020.asd

(defsystem #:advent2020
  :description "Advent of Code 2020"
  :author "Paul Jewell <paul@teulu.org>"
  :license "GNU3"  ;; Check proper license attribution
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre #:fiveam #:memoize)
  :components ((:file "package")
               (:file "day1")
               (:file "day2")
               (:file "day3")
               (:file "day4")
               (:file "day5")
               (:file "day6")
               (:file "day7")
               (:file "day8")
               (:file "day9")
               (:file "day10")
               (:file "day11")
               (:file "day12")
               (:file "day13"))
  :in-order-to ((test-op (test-op #:advent2020/test))))

(defsystem #:advent2020/test
  :serial t
  :depends-on (#:advent2020
               #:fiveam)
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op _) (symbol-call :fiveam :run-all-tests)))
