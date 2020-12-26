(in-package :day16)

(defparameter input "~/Projects/advent-of-code-2020/input/day16-input.txt")
(defparameter test-input "~/Projects/advent-of-code-2020/input/day16-test-input.txt")



(defun parse-rule (rule-str)
  (let* ((pos-colon (position #\: rule-str))
         (ranges (subseq rule-str (+ 2 pos-colon)))
         (range-strs (mapcar (lambda (s) (split "\-" s)) (split " or " ranges))))
    (mapcar (lambda (r) (mapcar #'parse-integer r)) range-strs)))

(defun parse-input (file)
  (let* ((data (split "\\n\\n" (uiop:read-file-string file)))
         (rules (mapcar #'parse-rule (split "\\n" (first data))))
         (my-ticket (mapcar 'parse-integer (split "," (second (split "\\n" (second data))))))
         (near-tickets (mapcar (lambda (v)
                                 (mapcar #'parse-integer (split #\, v)))
                               (cdr (split "\\n" (third data))))))
    (list rules my-ticket near-tickets)))

(defun valid-range-p (value &rest ranges)
  (some (lambda (range) (<= (car range) value (cadr range))) ranges))

(defun check-ticket (ranges ticket)
  (remove-if (lambda (n) (notevery 'null (mapcar (lambda (r) (apply #'valid-range-p n r)) ranges))) ticket))

(defun check-tickets (ranges tickets)
  (apply #'nconc (mapcar (lambda (ticket) (check-ticket ranges ticket)) tickets)))

(defun part1 (file)
  (let* ((data (parse-input file))
         (ranges (first data))
         (tickets (third data)))
    (apply #'+ (check-tickets ranges tickets))))

(defun test1 ()
  (part1 test-input))

(defun solution1 ()
  (part1 input))


(defun valid-ticket-p (ticket ranges)
  (notevery #'null (mapcar (lambda (n)
                             (every #'null (mapcar (lambda (r) (apply #'valid-range-p n r)) ranges)))
                           ticket)))

(defun part2 (file)
  (let* ((data (parse-input file))
         (rules (first data))
         (my-ticket (second data))
         (tickets (third data))
         (ranges (loop :for rule in rules
                       :collect (parse-rule rule)))
         (ticket-vals
           (mapcar (lambda (ticket-val) (mapcar #'parse-integer ticket-val))
                   (mapcar (lambda (ticket) (split #\, ticket))
                           tickets)))
         (valid-tickets ()))
    ranges))


(defun db-parse-input (l)
  (let ((types (split (format nil "~%~%") l)))
    (list (loop :for c :in (split (format nil "~%") (CAR types))
                :collect
                (loop :for n :in (split " " c)
                      :for r := (split "-" n)
                      :if (= 2 (length r))
                        :collect (cons (parse-integer (CAR r))
                                       (parse-integer (CADR r)))))

          (mapcar 'parse-integer (split "," (CADR (split (format nil "~%") (CADR types)))))
          (mapcar (lambda (o) (mapcar 'parse-integer (split "," o))) (CDR (split (format nil "~%") (CADDR types)))))))
