(in-package :day15)

(defparameter input '(12 1 16 3 11 0))
(defparameter test-input '(0 3 6))

(defvar memory)

(defun load-initial-data (data)
  (loop :for turn :from 1
        :for value :in data
        :do (setf (gethash value memory) turn)))

(defun compute-next-number (num turn end)
  (cond ((= turn end) num)
        ((null (gethash num memory)) (setf (gethash num memory) turn)(compute-next-number 0 (1+ turn) end))
        (t (let ((next (- turn (gethash num memory))))
             (setf (gethash num memory) turn) (compute-next-number next (1+ turn) end)))))

(defun solution (input end)
  (setq memory (make-hash-table))
  (load-initial-data input)
  (compute-next-number 0 (1+ (hash-table-count memory)) end))

(defun test1 ()
  (solution test-input 2020))

(defun solution1 ()
  (solution input 2020))


(defun test2 ()
  (solution test-input 30000000))

(defun solution2 ()
  (solution input 30000000))
