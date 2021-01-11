(in-package :day18)

(defparameter +test1-input+ "2 * 3 + (4 * 5)")
(defparameter +test2-input+ "5 + (8 * 3 + 9 + 3 * 4 * 3)")
(defparameter +test3-input+ "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
(defparameter +test4-input+ "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
(defparameter day18-input "~/Projects/advent-of-code-2020/input/day18-input.txt")

(defun prepare-string (str)
  (read-from-string (concatenate 'string "(" str ")")))

(defun parse-input (input)
  (mapcar #'prepare-string input))

;;Kudos to David Barringer - I spent several hours working through this problem, but couldn't find a way
;;                           through. David created this excellent concise routine. Thanks for sharing!
(defun eval1 (expr)
  (cond ((= 1 (length expr)) (car expr))
        ((listp (car expr)) (eval1 (cons (eval1 (car expr)) (cdr expr))))
        ((listp (caddr expr)) (eval1 (nconc (list (car expr) (cadr expr) (eval1 (caddr expr))) (cdddr expr))))
        (t (eval1 (cons (funcall (cadr expr) (car expr) (caddr expr)) (cdddr expr))))))

(defun test1 ()
  (eval1 (prepare-string +test1-input+)))

(defun test2 ()
  (eval1 (prepare-string +test2-input+)))

(defun test3 ()
  (eval1 (prepare-string +test3-input+)))

(defun test4 ()
  (eval1 (prepare-string +test4-input+)))

(defun solution1 ()
  (apply #'+ (mapcar #'eval1 (parse-input (uiop:read-file-lines day18-input)))))

(defun eval2 (expr)
  (cond ((= 1 (length expr)) (car expr))
        ((listp (car expr)) (eval2 (cons (eval2 (car expr)) (cdr expr))))
        ((listp (caddr expr)) (eval2 (nconc (list (car expr) (cadr expr) (eval2 (caddr expr))) (cdddr expr))))
        ((eql '* (cadr expr)) (funcall (cadr expr) (eval2 (list (car expr))) (eval2 (cddr expr))))
        (t (eval2 (cons (funcall (cadr expr) (car expr) (caddr expr)) (cdddr expr))))))

(defun solution2 ()
  (apply #'+ (mapcar #'eval2 (parse-input (uiop:read-file-lines day18-input)))))

