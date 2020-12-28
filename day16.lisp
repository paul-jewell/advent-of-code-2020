(in-package :day16)

(defparameter input "~/Projects/advent-of-code-2020/input/day16-input.txt")
(defparameter test-input "~/Projects/advent-of-code-2020/input/day16-test-input.txt")
(defparameter test-input-2 "~/Projects/advent-of-code-2020/input/day16-test-input-2.txt")


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

;; Check if value is in the supplied ranges
(defun value-in-range-p (value &rest ranges)
  (some (lambda (range) (<= (car range) value (cadr range))) ranges))

;; Remove all ticket values which are valid
(defun remove-valid-values (ranges ticket)
  (remove-if (lambda (ticket-val)
               (notevery 'null
                         (mapcar (lambda (range)
                                   (apply #'value-in-range-p ticket-val range))
                                 ranges)))
             ticket))

;; Find all the invalid values in the tickets
(defun invalid-values (ranges tickets)
  (apply #'nconc (mapcar (lambda (ticket) (remove-valid-values ranges ticket)) tickets)))

(defun part1 (file)
  (let* ((data (parse-input file))
         (ranges (first data))
         (tickets (third data)))
    (apply #'+ (invalid-values ranges tickets))))

(defun test1 ()
  (part1 test-input))

(defun solution1 ()
  (part1 input))

(defun check-ticket-p (ranges ticket)
  (notevery #'null (mapcar (lambda (ticket-val)
                             (every 'null
                                    (mapcar (lambda (r) (apply #'value-in-range-p ticket-val r))
                                            ranges)))
                           ticket)))

(defun r-intersection (current others)
  (if (null others)
      current
      (r-intersection (intersection current (car others)) (cdr others))))

(defun check-tickets (ranges tickets)
  (remove-if (lambda (ticket) (check-ticket-p ranges ticket)) tickets))

(defun check-range (range tickets)
  (loop :for ticket :in tickets
        :collect (loop :for i :upto (1- (length ticket))
                       :if (apply 'value-in-range-p (nth i ticket) range)
                         :collect i)))

(defun check-ranges (ranges tickets)
  (loop :for r :in ranges
        :for res := (check-range r tickets)
        :collect (r-intersection (car res) (cdr res))))

(defun match-range (iterations result)
  (if (= 0 iterations)
      result
      (match-range (1- iterations)
                   (loop :for i upto (1- (length result))
                         :for r := (nth i result)
                         :if (and (listp r) (= 1 (length r)))
                           :nconc (nconc (mapcar (lambda (s)
                                                   (if (listp s)
                                                       (remove (car r) s)
                                                       s))
                                                 (subseq result 0 i))
                                         r
                                         (mapcar (lambda (s)
                                                   (if (listp s)
                                                       (remove (car r) s)
                                                       s))
                                                 (subseq result (1+ i))))))))

(defun part2 (file)
  (let* ((data (parse-input file))
         (rules (first data))
         (my-ticket (second data))
         (tickets (third data))
         (ranges (check-ranges rules (check-tickets rules tickets))))
    (apply '* (subseq (loop :for n :in (match-range (length ranges) ranges)
                            :collect (nth n my-ticket))
                      0 6))))

(defun solution2 ()
  (part2 input))
