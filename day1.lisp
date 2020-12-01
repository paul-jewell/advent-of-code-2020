# Read input into list

(defparameter day1-input "~/Projects/advent-of-code-2020/day1-input.txt")
(defparameter expenses (mapcar #'parse-integer (uiop:read-file-lines day1-input)))

(defun ex-val (n)
  ;; Return expense value at index n
  (nth n expenses))

(defun day1-a ()
  (let ((expense-length (1- (length expenses))))
    (loop for i
          from 0 to expense-length
          do (loop for j from (1+ i) to expense-length
                   when (= (+ (nth i expenses)
                              (nth j expenses))
                           2020)
                     return (format t "Value a: ~a~%Value b: ~a~%Product: ~a~%"
                                    (nth i expenses)
                                    (nth j expenses)
                                    (* (nth i expenses) (nth j expenses)))))))

(defun day1-b ()
  (let ((expense-length (1- (length expenses))))
    (loop for i
          from 0 to expense-length
          do (loop for j
                   from (1+ i) to expense-length
                   do (loop for k
                            from (1+ j) to expense-length
                            when (= (+ (nth i expenses)
                                       (nth j expenses)
                                       (nth k expenses))
                                    2020)
                              return (format t "Value a: ~a~%Value b: ~a~%Value c: ~a~%Product: ~a~%"
                                             (nth i expenses)
                                             (nth j expenses)
                                             (nth k expenses)
                                             (* (nth i expenses) (nth j expenses) (nth k expenses))))))))



