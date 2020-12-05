(in-package :advent2020)

(defparameter day5-input "~/Projects/advent-of-code-2020/input/day5-input.txt")
(defparameter boarding-passes (uiop:read-file-lines day5-input))

(defparameter day5-test-input-1 "~/Projects/advent-of-code-2020/input/day5-test-input-1.txt")
(defparameter test-boarding-passes (uiop:read-file-lines day5-test-input-1))

(defun seat-id (boarding-pass)
  (let ((row-min 0)
        (row-max 127)
        (col-min 0)
        (col-max 7))
    (loop :for c in (coerce boarding-pass 'list)
          :do (cond
                ((char= #\F c) (setq row-max (- row-max (floor (/ (- row-max row-min) 2)))))
                ((char= #\B c) (setq row-min (+ row-min (ceiling (/ (- row-max row-min) 2)))))
                ((char= #\R c) (setq col-min (+ col-min (ceiling (/ (- col-max col-min) 2)))))
                ((char= #\L c) (setq col-max (- col-max (floor (/ (- col-max col-min) 2)))))
                (t (format t "Error - incorrect character"))))
    (+ (* row-min 8) col-min)))

(defun day5/test1 ()
  (apply #'max (mapcar #'seat-id test-boarding-passes)))

(defun day5/solution1 ()
  (apply #'max (mapcar #'seat-id boarding-passes)))

(defun day5/solution2 ()
  (let* ((passes (sort (mapcar #'seat-id boarding-passes) #'<))
        (min-seat-id (apply #'min passes))
        (max-seat-id (apply #'max passes)))
    (loop :for i :upfrom min-seat-id :to max-seat-id
          :when (not (find i passes))
            return i)))
