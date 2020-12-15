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

;; This routine was written because I didn't know the function position existed!
;; An alternative approach is to find the minimum bus in the list:
;;   (let ((next-bus-time (mapcar (lambda (x) (- x (rem (car input) x))) (second (input)))))
;; and then find the position (ie which bus has this minimum wait):
;;   (position (apply 'min next-bus-time) next-bus-time)

(defun earliest-bus (buses &optional (min nil) (result nil))
  (if (null buses)
      result
      (if (or (null min) (< (reduce '* (car buses)) min))
          (earliest-bus (cdr buses) (reduce '* (car buses)) (car buses))
          (earliest-bus (cdr buses) min result))))

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
  (mapcar (lambda (s) (parse-integer s :junk-allowed t)) (split #\, (cadr (uiop:read-file-lines file)))))

(defun get-timestamp (timestamp increment buses)
  (cond ((null buses) timestamp) ;; No buses left to process
        ((null (car buses)) (get-timestamp (+ timestamp 1) increment (cdr buses))) ;; no bus here
        ((= 0 (rem timestamp (car buses))) ;; Current bus works for this timestamp
         (get-timestamp (+ timestamp 1) (lcm increment (car buses)) (cdr buses))) ;; check remaining buses
        (t (get-timestamp (+ timestamp increment) increment buses)))) ;; Check next timestamp

(defun day13/part2 (file)
  (let ((input (parse-input2 file)))
    (- (get-timestamp (car input) 1 input) (length input))))

(defun day13/test2 ()
  (day13/part2 day13-test-input))

(defun day13/solution2 ()
  (day13/part2 day13-input))
