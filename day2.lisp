(defpackage #:advent2020.day02
  (:use #:cl)
  (:export #:day2a #:day2b))

(in-package #:advent2020.day02)

(defparameter day2-input "~/Projects/advent-of-code-2020/input/day2-input.txt")

(defun parse-password (pass-str)
  ;; Password line format:
  ;; x - y c: password
  ;; character c must be present between x and y times
  (let* ((pos-- (position #\- pass-str))
         (pos-space (position #\SPACE pass-str)) ;; the first space!
         (pos-colon (position #\: pass-str))
         (min (parse-integer (subseq pass-str 0 pos--)))
         (max (parse-integer (subseq pass-str (1+ pos--) pos-space)))
         (pass-letter (coerce (string-trim " " (subseq pass-str pos-space pos-colon))
                              'character))
         (password (coerce (string-trim " " (subseq pass-str (1+ pos-colon)))
                           'list)))
    (list min max pass-letter password)))

(defparameter passwords (mapcar #'parse-password (uiop:read-file-lines day2-input)))

(defun a-valid-password-p (password)
  (let* ((min (first password))
         (max (second password))
         (letter (third password))
         (password (nth 3 password))
         (count (count letter password :test #'eq)))
    (if (and (>= count min)
             (<= count max))
        T
        nil)))

(defun day2a ()
  (format t
          "The number of correct passwords: ~a~%"
          (length (remove-if-not #'a-valid-password-p passwords))))

(defun xor (x y)
  (and (or x y)
       (not (and x y))))

(defun b-valid-password-p (passlist)
  (let* ((first-pos (first passlist))
         (second-pos (second passlist))
         (letter (third passlist))
         (password (nth 3 passlist))
         (first-letter (nth (1- first-pos) password))
         (second-letter (nth (1- second-pos) password)))
    (if  (xor (eq first-letter letter)
              (eq second-letter letter))
         T
         nil))) 

(defun day2b ()
  (format t
          "The number of correct passwords: ~a~%"
          (length (remove-if-not #'b-valid-password-p passwords))))
