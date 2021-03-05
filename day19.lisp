(in-package :day19)

(defparameter day19-test-input "~/Projects/advent-of-code-2020/input/day19-test-input.txt")
(defparameter day19-test-input2 "~/Projects/advent-of-code-2020/input/day19-test-input2.txt")
(defparameter day19-input "~/Projects/advent-of-code-2020/input/day19-input.txt")

;; These three functions need to be refactored / clarified
(defun parse-rule-body (str)
  (if (char= #\" (aref str 0))
      (aref str 1)
      (prb-helper (split " " str) nil nil)))

(defun prb-helper (rules lst sublst)
  (cond ((null rules) (reverse (cons (reverse sublst) lst)))
        ((char= #\| (aref (car rules) 0)) (prb-helper (cdr rules) (cons (reverse sublst) lst) nil))
        (t (prb-helper (cdr rules) lst (cons (parse-integer (car rules)) sublst)))))

(defun read-rules (stream)
  (multiple-value-bind (is-valid matches)
      (scan-to-strings "^(\\d+): (.+)$" (read-line stream))
    (when is-valid
      (let
          ((rule-num (parse-integer (aref matches 0)))
           (rule-body (parse-rule-body (aref matches 1))))
        (cons (cons rule-num rule-body)
              (read-rules stream))))))

(defun check-rule-seq (message rules-arr rule-seq)
  "Check MESSAGE is valid against RULE-SEQ.
   Input to this function should be a list of rules to be checked in 
   sequence. eg - '(1 2 3)"
  (cond
    ((equal message 'invalid) (list message))
    ((null rule-seq) (list message)) ;If no rule sequence to check, return the message.
    ((null message) (list 'invalid))
    (t (let ((result-lst (check-rule (list message) rules-arr (car rule-seq))))
         (loop :for msg in result-lst
               :append (check-rule-seq msg rules-arr (cdr rule-seq)))))))
    
(defun check-rule (message-lst rules-arr rule)
  "Check each message in MESSAGE-LST against RULE, defined in RULE-ARR.
   Return the list of results."
  (let ((rules (aref rules-arr rule))) ;; Decompose the rule into constituent parts
       (loop :for message in message-lst
             :with result-lst
             :do (cond
                   ((equal 'invalid message) (push 'invalid result-lst))
                   ((null message) (push 'invalid result-lst)) ; No more message, but rules still exist to satisfy
                   ((characterp rules)   ; This rule is a letter
                    (push (if (equal rules (car message))
                              (cdr message)
                              'invalid)
                          result-lst))
                   (t (loop :for rule-seq in rules
                            :do (setq result-lst (append (check-rule-seq message rules-arr rule-seq) result-lst)))))
             :finally (return result-lst))))

(defun check (rules message)
  (some #'null (check-rule (list message) rules 0)))

(defun parse-input (file)
  (let* ((stream (open file))
         (rules-lst (read-rules stream))
         (rules
           (loop :with array := (make-array (1+ (reduce #'max rules-lst :key #'car)))
                 :for r in rules-lst
                 :do (setf (aref array (car r)) (cdr r))
                 :finally (return array)))
         (messages
           (loop :for str := (read-line stream nil)
                 :while str
                 :collect (coerce str 'list))))
    (list rules messages)))

(defun part1 (file)
  (let* ((input (parse-input file))
         (rules (first input))
         (messages (second input)))
    (count 't (mapcar (lambda (m) (check rules m)) messages))))

(defun test1 ()
  (part1 day19-test-input))

(defun solution1 ()
  (part1 day19-input))

;;======================================================================
;; Part 2
;;----------------------------------------------------------------------

(defun part2 (file)
  (let* ((input (parse-input file))
         (rules (first input))
         (messages (second input)))
    ;; Modify rules 8 and 11 as described:
    (setf (aref rules 8) '((42) (42 8)))
    (setf (aref rules 11) '((42 31) (42 11 31)))
    (count 't (mapcar (lambda (m) (check rules m)) messages))))

(defun test2 ()
  (part2 day19-test-input2))

(defun solution2 ()
  (part2 day19-input))

;; Use this instead of check to see the valid messages
(defun check2 (rules message)
  (let ((result (some #'null (check-rule (list message) rules 0)))
        (rtn-val (check-rule (list message) rules 0)))
    (when result
          (format t "~%result: ~A~%" rtn-val)
          (format t "Valid Message: ~A~%~%" message))
    result))

;;Invalid message with test data set:
;; (a a a a b b a a a a b b a a a)
;; Should be 12 valid messages, but this one should be invalid. It currently validates.

(defun part2t (file)
  (let* ((input (parse-input file))
         (rules (first input))
         (messages (second input)))
;;    '((a a a a b b a a a a b b a a a))
    (setf (aref rules 8) '((42) (42 8)))
    (setf (aref rules 11) '((42 31) (42 11 31)))
    (mapcar (lambda (m) (check2 rules m)) messages)))
