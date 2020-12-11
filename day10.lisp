(in-package :advent2020)

(defparameter day10-test-input1 "~/Projects/advent-of-code-2020/input/day10-test-input-1.txt")
(defparameter day10-test-input2 "~/Projects/advent-of-code-2020/input/day10-test-input-2.txt")
(defparameter day10-input "~/Projects/advent-of-code-2020/input/day10-input.txt")

(defun part1 (filename)
  (let* ((adaptors (sort (mapcar #'parse-integer
                                 (uiop:read-file-lines filename))
                         #'<))
         (max-jolt (apply #'max adaptors))
         (data (append '(0) adaptors (list (+ max-jolt 3))))
         (one-jolt 0)
         (three-jolt 0))
    (loop :for x from 0 upto (- (length data) 2) 
          :for y from 1 upto (1- (length data))
          :do (if (= (- (nth  y data)
                        (nth  x data)) 1)
                  (setf one-jolt (1+ one-jolt))
                  (setf three-jolt (1+ three-jolt))))
    (* one-jolt three-jolt)))

(defun day10/test1 ()
  (part1 day10-test-input1))

(defun day10/test2 ()
  (part1 day10-test-input2)) ;; process day10-test-input-2


(defun day10/solution1 ()
  (part1 day10-input))

(defun find-combinations (remaining-adaptors)
  (when (= 1 (length remaining-adaptors))
    (return-from find-combinations 1))
  (loop :with start := (first remaining-adaptors)
        :for (val . rest) :on (rest remaining-adaptors)
        :while (<= (- val start) 3)
        :sum (find-combinations (cons val rest))))

(memoize-function 'find-combinations :test #'equal)

(defun read-data (filename)
  (cons 0 (sort (mapcar #'parse-integer (uiop:read-file-lines filename)) #'<)))

(defun day10/test3 ()
  (find-combinations (read-data day10-test-input1)))

(defun day10/test4 ()
  (find-combinations (read-data day10-test-input2)))


(defun day10/solution2 ()
  (find-combinations (read-data day10-input)))






