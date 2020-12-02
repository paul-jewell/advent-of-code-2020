(defpackage #:advent2020.day01
  (:use #:cl)
  (:export #:day1a #:day1b))

(in-package #:advent2020.day01)

;; Read input into list
(defparameter day1-input "~/Projects/advent-of-code-2020/day1-input.txt")
(defparameter expenses (mapcar #'parse-integer (uiop:read-file-lines day1-input)))

;; Refactor - first version - credit and thanks to samuel-hunter (on github)
;; My observations/learning from this:
;;  - Keywording the loop elements makes clearer
;;  - Loop keyword :on 
;;  - So much clearer than my first version!

(defun day1a ()
    (loop :for nums :on expenses
          :for x := (first nums)
          :do (loop :for y :in (rest nums)
                    :when (= 2020 (+ x y))
                      :do (return-from day1a
                            (values (* x y) x y)))))

(defun day1b ()
  (loop :for nums :on expenses
        :for x := (first nums)
        :do (loop :for nums2 :on (rest nums)
                  :for y := (first nums2)
                  :do (loop :for z :in (rest nums2)
                            :when (= 2020 (+ x y z))
                              :do (return-from day1b
                                    (values (* x y z) x y z))))))






