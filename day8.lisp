(in-package :advent2020)

(defparameter day8-test-input "~/Projects/advent-of-code-2020/input/day8-test-input.txt")
(defparameter day8-input "~/Projects/advent-of-code-2020/input/day8-input.txt")

(defstruct console
  code
  (acc 0)
  (pc 0))

(defmacro create-console (name code)
  `(setq ,(eval name) (make-console :code ,code)))

;; This is my implementation 
(defun parse-program (program-file)
  (let ((code (uiop:read-file-lines program-file)))
    (loop :for line :in code
          collect (destructuring-bind (opcode operand)
                      (split "\\s" line)
                    (list (read-from-string opcode) (parse-integer operand))))))

(defun run-console1 (name visitlist)
  (cond ((null (find (console-pc name) visitlist))
         (setf visitlist (cons (console-pc name) visitlist))
         (funcall (car (nth (console-pc name) (console-code name)))
                  name (cdr (nth (console-pc name) (console-code name))))
         (run-console1 name visitlist))
        (t (console-acc name))))

(defun nop (name in)
  (declare (ignore in))
  (setf (console-pc (eval name)) (+ 1 (console-pc (eval name)))))

(defun acc (name in)
  (setf (console-acc (eval name)) (+ (car in) (console-acc (eval name))))
  (setf (console-pc (eval name)) (+ 1 (console-pc (eval name)))))

(defun jmp (name in)
  (setf (console-pc (eval name)) (+ (car in) (console-pc (eval name)))))

(defun day8/test1 ()
  (run-console1 (create-console (gensym) (parse-program day8-test-input)) nil))

(defun day8/solution1 ()
  (run-console1 (create-console (gensym) (parse-program day8-input)) nil))

;; Part b specific code below

(defun replace-code (code old-opcode new-opcode)
  (loop for i from 0 to (- (length code) 1)
        if (equal old-opcode (car (nth i code)))
          collect (nconc (subseq code 0 i) (list (cons new-opcode (cdr (nth i code)))) (nthcdr (+ i 1) code))))

(defun run-console2 (name visitlist)
  (cond ((null (nth (console-pc name) (console-code name))) (console-acc name))
        ((null (find (console-pc name) visitlist))
         (setf visitlist (cons (console-pc name) visitlist))
         (funcall (car (nth (console-pc name) (console-code name))) name (cdr (nth (console-pc name) (console-code name))))
         (run-console2 name visitlist))
        (t nil)))

;; (defun day8/part2/ (program-file)
;;   (let ((input (parse-program program-file)))
;;     (car (remove nil (loop :for program :in (nconc (replace-code input 'jmp 'nop)
;;                                                     (replace-code input 'nop 'jmp))
;;                             :collect (run-console2 (create-console (gensym) program) nil))))))


;; version without macro
(defvar my-cons)
(defun day8/part2 (program-file)
  (let ((input (parse-program program-file)))
    (car (remove nil (loop :for program :in (nconc (replace-code input 'jmp 'nop)
                                                    (replace-code input 'nop 'jmp))
                           :collect (run-console2 (setq my-cons (make-console :code program)) nil))))))


(defun day8/test2 ()
  (day8/part2 day8-test-input))

(defun day8/solution2 ()
  (day8/part2 day8-input))

