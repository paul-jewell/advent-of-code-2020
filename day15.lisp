(in-package :day15)

(defparameter input '(12 1 16 3 11 0))
(defparameter test-input '(0 3 6))

(defun save-turn (numbers value turn)
  (setf (gethash value numbers) (cons turn (gethash value numbers))))

(defun next-turn (numbers last-number turn)
  (let ((turns (gethash last-number numbers)))
    (if (< (length turns) 2)
        0
        (- (first (gethash last-number numbers))
           (second (gethash last-number numbers))))))

(defun compute-number (v end)
  (let ((numbers (make-hash-table))
        (last-number 0))
    ;; Load hash with start numbers
    (loop :for n :in v
          :for turn :from 1 to (length v)
          :do (setf (gethash n numbers) (cons turn (gethash n numbers))))
    (setq last-number (car (last v)))
    ;; Calculate following numbers
    (loop :for turn :from (1+ (length v)) to end
          :do (progn
                (setq last-number (next-turn numbers last-number turn))
                (save-turn numbers last-number turn))
          :finally
             (return last-number))))

(defun test1 ()
  (compute-number '(0 3 6) 2020))

(defun solution1 ()
  (compute-number input 2020))
