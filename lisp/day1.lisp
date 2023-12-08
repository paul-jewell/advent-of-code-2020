(in-package #:day1)

;; Read input into list
(defparameter day1-input "~/Projects/advent-of-code-2020/input/day1-input.txt")
(defparameter expenses (mapcar #'parse-integer (uiop:read-file-lines day1-input)))

;; Refactor - second version - credit and thanks to bpanthi on lisp discord
;; My observations/learning from this:
;;  - Keywording the loop elements makes clearer
;;  - Loop keyword :on 
;;  - So much clearer than my first version!

(defun solution1 ()
  (loop :for (a . tail) on expenses
        :for b = (- 2020 a)
        :when (find b tail)
          return (* a b)))

(defun solution2 ()
  (loop :for (a . tail) on expenses
        :do (loop :for (b . b-tail) :on tail
                  :for c := (- 2020 (+ a b))
                  :when (find c b-tail)
                    :do (return-from solution2
                          (* a b c)))))




