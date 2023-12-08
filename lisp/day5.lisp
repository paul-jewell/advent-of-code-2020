(in-package :day5)

(defparameter day5-input "~/Projects/advent-of-code-2020/input/day5-input.txt")
(defparameter boarding-passes (uiop:read-file-lines day5-input))

(defparameter day5-test-input-1 "~/Projects/advent-of-code-2020/input/day5-test-input-1.txt")
(defparameter test-boarding-passes (uiop:read-file-lines day5-test-input-1))

;; The boarding pass is essentially a binary representation, so using this information, we can
;; simplify the process of finding the seat-id
;; - This was seen in the video from Mike Zemanski: https://www.youtube.com/watch?v=I8dbKJ_315Q
;;   although he is coding in clojure. Thanks Mike, and those commenters who pointed this out to him!
(defun seat-id (boarding-pass)
  (let ((vals '((#\B 1) (#\F 0) (#\R 1) (#\L 0))))
    (loop :for i :from 0
          :for c in (mapcar #'(lambda (c) (cadr (assoc c vals))) (reverse (coerce boarding-pass 'list)))
          :sum (* (expt 2 i) c))))

(defun test1 ()
  (apply #'max (mapcar #'seat-id test-boarding-passes)))

(defun solution1 ()
  (apply #'max (mapcar #'seat-id boarding-passes)))

(defun solution2 ()
  (let* ((passes (sort (mapcar #'seat-id boarding-passes) #'<))
        (min-seat-id (apply #'min passes))
        (max-seat-id (apply #'max passes)))
    (loop :for i :upfrom min-seat-id :to max-seat-id
          :when (not (find i passes))
            return i)))
