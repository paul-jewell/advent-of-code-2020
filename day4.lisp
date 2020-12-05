(in-package :advent2020)

(defparameter day4-test-input-1 "~/Projects/advent-of-code-2020/input/day4-test-input.txt")
;; Test input 2 containts 4 valid and 4 invalid passports according to part 2 criteria
(defparameter day4-test-input-2 "~/Projects/advent-of-code-2020/input/day4-test-input-2.txt")

(defparameter day4-input "~/Projects/advent-of-code-2020/input/day4-input.txt")

(defun parse-passports (in &optional (passports '()) (curr-passport ""))
  (let ((line (read-line in nil)))
    (if (not (null line))
        (if (= (length line) 0)
            (parse-passports in (append passports (list curr-passport)))
            (parse-passports in passports (concatenate 'string curr-passport line " ")))
        (append passports (list curr-passport)))))

(defun parse-passport (pass-str)
  (let ((pass-tokens (split "\\s+" pass-str)))
    (loop :for s in pass-tokens
          collect (progn
                    (let ((key (intern (string-upcase (subseq s 0 3)) "KEYWORD"))
                          (value (subseq s 4)))
                      (list key value))))))

(defun parse-input (file)
  (let* ((passports (mapcar #'parse-passport (with-open-file (in file)
                                               (parse-passports in)))))
    passports))

(defun passport-valid-1-p (passport)
  (and (not (null (assoc :BYR passport)))
       (not (null (assoc :IYR passport)))
       (not (null (assoc :EYR passport)))
       (not (null (assoc :HGT passport)))
       (not (null (assoc :HCL passport)))
       (not (null (assoc :ECL passport)))
       (not (null (assoc :PID passport)))
       ;; CID can be missing
       ))

;;; Additional validation functions for solution 2

(defun validate-date-p (date-str min max)
  (<= min (parse-integer date-str) max))

(defun validate-height-p (hgt)
  (let* ((unit-pos (- (length hgt) 2))
         (unit (subseq hgt unit-pos))
         (height (parse-integer (subseq hgt 0 unit-pos))))
    (cond
      ((equal unit "in") (<= 59 height 76))
      ((equal unit "cm") (<= 150 height 193))
      (T nil))))

(defun validate-eye-colour-p (ecl)
 (or (equal ecl "amb")
     (equal ecl "blu")
     (equal ecl "brn")
     (equal ecl "gry")
     (equal ecl "grn")
     (equal ecl "hzl")
     (equal ecl "oth")))

(defun validate-hair-colour-p (hcl)
  (and (eql #\# (char hcl 0))
       (eql (length hcl) 7)  ;; # char followed by 6
       (let ((colour (subseq hcl 1)))
         (subsetp (coerce colour 'list)
                  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0 #\a #\b #\c #\d #\e #\f)))))

(defun validate-passport-id-p (pid)
  (and (= (length pid) 9)
       (subsetp (coerce pid 'list) '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))

(defun passport-valid-2-p (passport)
  (let ((byr (cadr (assoc :BYR passport)))
        (iyr (cadr (assoc :IYR passport)))
        (eyr (cadr (assoc :EYR passport)))
        (hgt (cadr (assoc :HGT passport)))
        (hcl (cadr (assoc :HCL passport)))
        (ecl (cadr (assoc :ECL passport)))
        (pid (cadr (assoc :PID passport))))

    (and (passport-valid-1-p passport)
         (validate-date-p byr 1920 2002)
         (validate-date-p iyr 2010 2020)
         (validate-date-p eyr 2020 2030)
         (validate-height-p hgt)
         (validate-hair-colour-p hcl)
         (validate-eye-colour-p ecl)
         (validate-passport-id-p pid))))
       ;; CID can be missing

(defun day4/test1 ()
  (count-if #'passport-valid-1-p (parse-input day4-test-input-1)))

(defun day4/solution1 ()
  (count-if #'passport-valid-1-p (parse-input day4-input)))

(defun day4/test2 ()
  (count-if #'passport-valid-2-p (parse-input day4-test-input-2)))

(defun day4/solution2 ()
  (count-if #'passport-valid-2-p (parse-input day4-input)))
