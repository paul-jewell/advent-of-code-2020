(in-package :advent2020)

(defparameter day9-test-input "~/Projects/advent-of-code-2020/input/day9-test-input.txt")
(defparameter day9-input "~/Projects/advent-of-code-2020/input/day9-input.txt")

(defun sums-in-preamble (list)
  (let ((result nil))
    (loop :for values on list
          :for x := (first values)
          :do (loop :for y :in (rest values)
                    :when (/= x y)
                      :do (setq result (cons (+ x y) result))))
    result))

(defun part1 (filename preamble)
  (let ((data (mapcar #'parse-integer (uiop:read-file-lines filename))))
    (loop :for i from preamble
            :to (1- (length data))
          collect (if (find (nth i data) (sums-in-preamble (subseq data (- i preamble) i)))
                      nil
                      (nth i data)))))

(defun day9/test1 ()
  (car (remove-if #'null (part1 day9-test-input 5))))

(defun day9/solution1 ()
  (car (remove-if #'null (part1 day9-input 25))))

(defun day9/solution2 ()
  )



