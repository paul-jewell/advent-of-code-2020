(in-package :day19)

(defparameter day19-test-input "~/Projects/advent-of-code-2020/input/day19-test-input.txt")
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
  (if (null rule-seq) ; RULE-SEQ can be nil if the rule set didn't have two branches
        message       ;...so return the message.
        (let ((result (check-rule message rules-arr (car rule-seq)))) ; check the first rule
          (cond                                                       ; in the sequence
            ((equal result 'invalid)   ; message doesn't comply with rule
             'invalid)                 ;...so return 'invalid
            (t (check-rule-seq result rules-arr (cdr rule-seq)))))))  ; Check the rest of the
                                                                      ; sequence recursively
(defun check-rule (message rules-arr rule)
  "Check MESSAGE against RULE-NUM.
   Input to this function should only be a single rule number."
  (let ((rules (aref rules-arr rule))) ; Decompose rule 
    (cond
      ((null message) 'invalid) ; No more message left
      ((characterp rules)       ; Rule is letter
       (if (char= rules (car message))
             (cdr message)      ;...if matches - consume matched and return rest
             'invalid))         ;...or 'invalid if not.
      (t (let ((result (check-rule-seq message rules-arr (first rules))))
           (cond
             ((equal result 'invalid) ;...first leg invalid, so check second leg
              (check-rule-seq message rules-arr (second rules)))
             (t result)))))))   ;...first leg is valid, no need to check second leg

(defun check (rules message)
  (null (check-rule message rules 0)))

(defun part1 (file)
  (let* ((stream (open file))
         (rules-lst  (read-rules stream))
         (rules
           (loop :with array := (make-array (1+ (reduce #'max (mapcar #'car rules-lst))))
                 :for r :in rules-lst
                 :do (setf (aref array (car r)) (cdr r))
                 :finally (return array)))
         (messages
           (loop :for str := (read-line stream nil)
                 :while str
                 :collect (coerce str 'list))))
    ;(mapcar (lambda (m) (check rules m)) messages)
    (count 't (mapcar (lambda (m) (check rules m)) messages))))

(defun test1 ()
  (part1 day19-test-input))

(defun solution1 ()
  (part1 day19-input))
