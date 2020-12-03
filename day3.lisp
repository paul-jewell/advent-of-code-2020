(defpackage #:advent2020.day03
  (:use #:cl)
  (:export #:day3a #:day3b))

(in-package #:advent2020.day03)

(defparameter day3-input "~/Projects/advent-of-code-2020/input/day3-input.txt")

(defun parse-line (tree-line)
  (coerce tree-line 'list))

(defparameter tree-lines (mapcar #'parse-line (uiop:read-file-lines day3-input)))

(defparameter trees (make-array (list (length tree-lines) (length (first tree-lines)))
                                :initial-contents tree-lines))

(defun tree-count (row-inc col-inc)
  (length (loop :for r :below (first (array-dimensions trees)) :by row-inc
                :for c :from 0 :by col-inc
                :when (eq (aref trees r (mod c 31)) #\#) ;; we have a tree...
                  collect (list (aref trees r (mod c 31))))))

(defun day3a ()
  (tree-count 1 3))

(defun day3b ()
  (* (tree-count 1 1)
     (tree-count 1 3)
     (tree-count 1 5)
     (tree-count 1 7)
     (tree-count 2 1)))
