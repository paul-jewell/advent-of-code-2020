(in-package :advent2020)

(defparameter day8-test-input "~/Projects/advent-of-code-2020/input/day8-test-input.txt")
(defparameter day8-input "~/Projects/advent-of-code-2020/input/day8-input.txt")

(defun load-console (code)
  (let ((pc 0)
        (acc 0)
        (program code))
    (list pc acc (loop for line in program
                        collect (destructuring-bind (opcode operand)
                                    (split "\\s" line)
                                  (list opcode (parse-integer operand)))) '())))

(defun load-program (code)
  (let ((pc 0)
        (acc 0))
    (list pc acc code '())))

(defun update-history (console)
  (push (car console) (cadddr console))
  console)

(defun advance-pc (console &optional (jmp 1))
  (let ((console (update-history console))) ;; track visited instructions
    (setf (car console) (+ (car console) jmp))
    console))

(defun nop (console)
  (advance-pc console)
  console)

(defun jmp (console operand)
  (advance-pc console operand)
  console)

(defun acc (console operand)
  (setf (cadr console) (+ (cadr console) operand))
  (advance-pc console)
  console)

(defun run1 (console)
  (if (not (member (car console) (cadddr console)))
      (let* ((instruction (nth (car console) (caddr console)))
             (opcode (car instruction))
             (operand (cadr instruction)))
        (cond
          ((equal opcode "nop") (nop console))
          ((equal opcode "jmp") (jmp console operand))
          ((equal opcode "acc") (acc console operand)))
        (run1 console))
      console))



(defun run2 (console)
  (format t "Console: ~S~%" console)
  (if (and (not (> (car console) (length (caddr console))))
           (not (member (car console) (cadddr console))))
      (let* ((instruction (nth (car console) (caddr console)))
             (opcode (car instruction))
             (operand (cadr instruction)))
        (format t "instruction: ~S :: ~S : ~S~%" instruction opcode operand)
        (cond
          ((equal opcode "nop") (nop console))
          ((equal opcode "jmp") (jmp console operand))
          ((equal opcode "acc") (acc console operand)))
        (run2 console))
      (if (> (car console) (length (caddr console)))
          (values (cadr console) console)
          (values nil console))))


(defun day8/test1 ()
  (cadr (run1 (load-console (uiop:read-file-lines day8-test-input)))))

(defun day8/solution1 ()
  (cadr (run1 (load-console (uiop:read-file-lines day8-input)))))



