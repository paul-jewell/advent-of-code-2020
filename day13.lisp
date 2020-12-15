(in-package :advent2020)

(defparameter day13-test-input "~/Projects/advent-of-code-2020/input/day13-test-input.txt")
(defparameter day13-input "~/Projects/advent-of-code-2020/input/day13-input.txt")


(defun parse-input (file)
  (let* ((input (uiop:read-file-lines file))
        (start-time (parse-integer (car input)))
        (buses (mapcar #'parse-integer
                       (remove-if (lambda (s) (string= s "x"))
                                  (split #\, (cadr input))))))
    (list start-time buses)))

(defun earliest-bus (buses &optional (min nil) (result nil))
  (if (null buses)
      result
      (if (or (null min) (< (reduce '* (car buses)) min))
          (min-bus (cdr buses) (reduce '* (car buses)) (car buses))
          (min-bus (cdr buses) min result))))

(defun day13/part1 (file)
  (let* ((input (parse-input file))
         (bus-times (mapcar (lambda (x) (list x (ceiling (/ (car input) x))))
                            (cadr input)))
         (my-bus (earliest-bus bus-times)))
    (* (car my-bus) (- (reduce '* my-bus) ;; bus arrival time
                       (car input)))))    ;; my arrival time

(defun day13/test1 ()
  (day13/part1 day13-test-input))

(defun day13/solution1 ()
  (day13/part1 day13-input))


(defun parse-input2 (file)
  (remove-if #'null (loop :for v in (split #\, (second (uiop:read-file-lines file)))
                          :for i from 0
                          collect (list i (parse-integer v :junk-allowed t))) :key #'cadr))

(defun timestamp-valid-p (timestamp bus)
  (= (rem (+ timestamp (car bus)) (cadr bus)) 0))

(defun max-bus (buses &optional (max nil) (result nil)) ;; Find the bus with the largest interval
  (if (null buses)
      result
      (if (or (null max) (> (cadar buses) max))
          (max-bus (cdr buses) (cadar buses) (car buses))
          (max-bus (cdr buses) max result))))

(defun day13/part2 (input)
  (let* ((buses (sort input #'> :key 'cadr))
         (max-bus (max-bus buses)))
    (loop :for timestamp :from (- (cadr max-bus) (car max-bus)) :by (cadr max-bus)
          :when (every (lambda (bus) (= (rem (+ timestamp (car bus)) (cadr bus)) 0)) buses)
            return timestamp)))


(defun day13/test2 ()
  (day13/part2 (parse-input2 day13-test-input)))

(defun day13/solution2 ()
  (day13/part2 (parse-input2 day13-input)))
