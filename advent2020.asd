;;;; advent2020.asd

(asdf:defsystem #:advent2020
  :description "Advent of Code 2020"
  :author "Paul Jewell <paul@teulu.org"
  :license "GNU3"  ;; Check proper license attribution
  :version "0.0.1"
  :serial t ;; TODO: Find out what this does??
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
               (:file "day1")
               (:file "day2")
               (:file "day3")
               (:file "day4")
               (:file "day5"))
  :in-order-to ((test-op (test-op #:advent2020/test))))

(asdf:defsystem #:advent2020/test
  :depends-on (#:advent2020
               #:fiveam)
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op _) (symbol-call :fiveam :run-all-tests)))
