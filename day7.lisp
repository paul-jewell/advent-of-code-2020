(in-package :day7)

(defparameter day7-test-input "~/Projects/advent-of-code-2020/input/day7-test-input.txt")
(defparameter day7-input "~/Projects/advent-of-code-2020/input/day7-input.txt")

(defparameter test-input (uiop:read-file-lines day7-test-input))

;; Disclosure - I worked on this all day, but failed to find a way through by myself.
;; This code is basically from larsen's submission. Boy, do I have a lot to learn...!

(defun read-bag-rules (rule-file)
  (let ((graph (make-hash-table :test 'equal)))
    (loop :for rule :in (uiop:read-file-lines rule-file)
          :do (register-groups-bind (colour rest-of-rule)
                  ("^(\\w+ \\w+) bags contain \(.*\)" rule)
                (setf (gethash colour graph)
                      (loop for rule-part in (split "," rest-of-rule)
                            collect (register-groups-bind ((#'parse-integer n) c-colour)
                                        ("\(\\d+\) \(\\w+ \\w+\) bag" rule-part)
                                      (cons c-colour n))))))
    graph))

(defun path (graph start end)
  (labels ((path-using (graph start end node-list)
               (if (equal start end)
                   (return-from path (car (reverse node-list)))
                   (dolist (child (mapcar #'car (gethash start graph)))
                     (path-using graph child end (cons child node-list))))))
    (path-using graph start end (list start))))

;; TODO: The above path function uses a sub routine to break out of the dolist
;; with the result. I need to see if I can do it here without using the subroutine
(defun path2 (graph start end &optional (node-list (list start)))
  (princ node-list)
  (if (equal start end)
         (return-from path2 (car (reverse node-list)))
         (dolist (child (mapcar #'car (gethash start graph))) 
           (path2 graph child end (cons child node-list)))))

(defun sol1 (file)
  (let ((graph (read-bag-rules file))
        (solutions 0))
    (loop :for colour being the hash-keys of graph
          :when (and (not (string= colour "shiny gold"))
                     (path graph colour "shiny gold"))
            :do (incf solutions)
          :finally (return solutions))))

(defun test1 ()
  (sol1 day7-test-input))

(defun solution1 ()
  (sol1 day7-input))

(defun total-bags (start bag-rules)
  (+ 1 (loop for (color . quantity) in (gethash start bag-rules)
             when (numberp quantity)
               sum (* quantity (total-bags color bag-rules)))))

(defun sol2 (rule-file)
  (- (total-bags "shiny gold" (read-bag-rules rule-file)) 1))

(defun test2 ()
  (sol2 day7-test-input))

(defun solution2 ()
  (sol2 day7-input))


