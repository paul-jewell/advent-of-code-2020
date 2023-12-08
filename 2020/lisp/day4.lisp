(in-package :day4)

(defparameter day4-test-input-1 "~/Projects/advent-of-code-2020/input/day4-test-input.txt")
;; Test input 2 containts 4 valid and 4 invalid passports according to part 2 criteria
(defparameter day4-test-input-2 "~/Projects/advent-of-code-2020/input/day4-test-input-2.txt")

(defparameter day4-input "~/Projects/advent-of-code-2020/input/day4-input.txt")

(defun read-passports (passport-file)
  (split "\\n\\n" (uiop:read-file-string passport-file)))

(defun passport-valid-1-p (passport)
  (let ((fields-in-passport (loop :for field :in (split "\\s" passport)
                                  :collect (first (split ":" field)))))
    (every (lambda (f)
             (find f fields-in-passport :test #'string=))
           '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))))

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
  (find ecl '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string=))

(defun validate-hair-colour-p (hcl)
  (scan "^#[a-f0-9]{6}$" hcl))

(defun validate-passport-id-p (pid)
  (scan "^\\d{9}$" pid))

(defun passport-valid-2-p (passport-str)
  (let ((passport (make-hash-table :test 'equal)))
    (loop :for (key . value) :in (mapcar (lambda (f)
                                         (split ":" f))
                                       (split "\\s" passport-str))
          :do (setf (gethash key passport) (first value)))
    (and (passport-valid-1-p passport-str)
         (validate-date-p (gethash "byr" passport) 1920 2002)
         (validate-date-p (gethash "iyr" passport) 2010 2020)
         (validate-date-p (gethash "eyr" passport) 2020 2030)
         (validate-height-p (gethash "hgt" passport))
         (validate-hair-colour-p (gethash "hcl" passport))
         (validate-eye-colour-p (gethash "ecl" passport))
         (validate-passport-id-p (gethash "pid" passport)))))

(defun test1 ()
  (count-if #'passport-valid-1-p (read-passports day4-test-input-1)))

(defun solution1 ()
  (count-if #'passport-valid-1-p (read-passports day4-input)))

(defun test2 ()
  (count-if #'passport-valid-2-p (read-passports day4-test-input-2)))

(defun solution2 ()
  (count-if #'passport-valid-2-p (read-passports day4-input)))
