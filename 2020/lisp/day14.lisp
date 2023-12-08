(in-package :day14)

(defparameter day14-input "~/Projects/advent-of-code-2020/input/day14-input.txt")
(defparameter day14-test-input "~/Projects/advent-of-code-2020/input/day14-test-input.txt")
(defparameter day14-test-input-2 "~/Projects/advent-of-code-2020/input/day14-test-input-2.txt")

(defstruct d-comp
  code
  (mask nil)
  (PC 0)
  (memory (make-hash-table)))

(defun new-value (value mask)
  (parse-integer  (coerce (loop :for m in (coerce mask 'list)
                                :for v in (coerce (format nil "~36,'0B" value) 'list)
                                :collect (if (char= m #\X) v m))
                          'string)
                  :radix 2))

(defun parse-input (file)
  (let ((input (uiop:read-file-lines file)))
    (loop :for line :in input
          :collect (split " = " line))))


(defun run-program (computer)
  (loop :for (opcode . operand) :in (d-comp-code computer)
        :do (cond ((string= opcode "mask") (setf (d-comp-mask computer) (car operand)))
                  (T (let ((mem-location (parse-integer (subseq opcode 4) :junk-allowed T))
                           (masked-value (new-value (parse-integer (car operand)) (d-comp-mask computer))))
                       (setf (gethash mem-location (d-comp-memory computer)) masked-value)))))
  computer)

(defun part1 (file)
  (let ((computer (make-d-comp :code (parse-input file))))
    (run-program computer)
    (loop :for v :being :the :hash-values :of (d-comp-memory computer)
          :sum v)))

(defun test1 ()
  (part1 day14-test-input))

(defun solution1 ()
  (part1 day14-input))

;; Mask and address are already lists...
(defun store-val (computer mask address value &optional (index 0))
  (if (= (length mask) index)
      (setf (gethash (parse-integer (coerce address 'string)) (d-comp-memory computer)) value)
      (cond
        ((char= #\0 (nth index mask)) ;; address value remains
         (store-val computer mask address value (1+ index)))
        ((char= #\1 (nth index mask)) ;; address value set to 1
         (store-val computer
                    mask
                    (nconc (subseq address 0 index) (list #\1) (nthcdr (+ index 1) address))
                    value
                    (1+ index)))
        ((char= #\X (nth index mask))
         (progn (store-val computer
                           mask
                           (nconc (subseq address 0 index) (list #\0) (nthcdr (+ index 1) address))
                           value
                           (1+ index))
                (store-val computer
                           mask
                           (nconc (subseq address 0 index) (list #\1) (nthcdr (+ index 1) address))
                           value
                           (1+ index)))))))

(defun run-program-2 (computer)
  (loop :for (opcode . operand) :in (d-comp-code computer)
        :do (cond ((string= opcode "mask") (setf (d-comp-mask computer) (coerce (car operand) 'list)))
                  (T (store-val computer
                                (d-comp-mask computer)
                                (coerce (format nil "~36,'0B" (parse-integer (subseq opcode 4) :junk-allowed T)) 'list)
                                (parse-integer (car operand))))))
  computer)

(defun part2 (file)
  (let ((computer (make-d-comp :code (parse-input file))))
    (run-program-2 computer)
    (loop :for v :being :the :hash-values :of (d-comp-memory computer)
          :sum v)))

(defun test2 ()
  (part2 day14-test-input-2))

(defun solution2 ()
  (part2 day14-input))

