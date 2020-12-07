(in-package :advent2020)

(defparameter day6-test-input "~/Projects/advent-of-code-2020/input/day6-test-input.txt")
(defparameter day6-input "~/Projects/advent-of-code-2020/input/day6-input.txt")

(defun count-yes (response)
  (length (remove-duplicates
           (remove-if #'(lambda (c)
                          (char= c #\newline))
                      (coerce response 'list)))))

(defun day6/sol1 (input-file)
  (apply #'+ (mapcar #'count-yes
                     (mapcar #'(lambda (s)
                                 (coerce s 'list))
                             (split "\\n\\n" (uiop:read-file-string input-file))))))

(defun day6/test1 ()
  (day6/sol1 day6-test-input))

(defun day6/solution1 ()
  (day6/sol1 day6-input))

(defun common-response-count (response-list)
  (length (if (= (length response-list) 1)
              (car response-list)
              (reduce #'intersection response-list))))

(defun day6/sol2 (input-file)
  (apply #'+
         (mapcar 'common-response-count
                 (mapcar #'(lambda (group)
                             (mapcar #'(lambda (element)
                                         (coerce element 'list)) 
                                     group))
                         (mapcar #'(lambda (s)
                                     (split "\\n" s))
                                 (split "\\n\\n" (uiop:read-file-string input-file)))))))


(defun day6/test2 ()
  (day6/sol2 day6-test-input))

(defun day6/solution2 ()
  (day6/sol2 day6-input))

