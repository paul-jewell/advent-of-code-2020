(in-package :advent2020)

(defparameter day11-input "~/Projects/advent-of-code-2020/input/day11-input.txt")
(defparameter day11-test-input "~/Projects/advent-of-code-2020/input/day11-test-input.txt")

(defun parse-layout (filename)
  (let ((file-lines (uiop:read-file-lines filename)))
    (make-array (list (length file-lines) (length (first file-lines)))
                :initial-contents file-lines)))

(defun occupy-seat (layout x y)
  (setf (aref layout x y) #\#))

(defun empty-seat (layout x y)
  (setf (aref layout x y) #\L))

(defun occupied-p (layout x y)
  (char= (aref layout x y) #\#))

(defun empty-p (layout x y)
  (and (not (occupied-p layout x y))
       (not (floor-p layout x y))))

(defun floor-p (layout x y)
  (char= (aref layout x y) #\.))

(defun count-occupied-seats (layout x-seat y-seat)
  (count-if-not #'null (let ((x-min (max (1- x-seat) 0))
                   (x-max (min (1+ x-seat) (1- (first (array-dimensions layout)))))
                   (y-min (max (1- y-seat) 0))
                   (y-max (min (1+ y-seat) (1- (second (array-dimensions layout))))))
               (loop :for x :from x-min :to x-max
                     append (loop :for y :from y-min :to y-max
                                  :if (not (and (= x-seat x)
                                                (= y-seat y)))
                                    collect (occupied-p layout x y))))))

(defun seat-change1 (start-seats)
  (let ((new-layout (make-array  (array-dimensions start-seats))) ;; May have to make new array to avoid corrupting original
        (changed-p nil))
    (loop :for x :below (first (array-dimensions start-seats))
          :do (loop :for y :below (second (array-dimensions start-seats))
                    :do (cond
                          ((and (empty-p start-seats x y)
                                (= (count-occupied-seats start-seats x y) 0))
                           (progn
                             (occupy-seat new-layout x y)
                             (setf changed-p T)))
                                        ; Seat: occupied -> empty 
                          ((and (occupied-p start-seats x y)
                                (>= (count-occupied-seats start-seats x y) 4))
                           (progn
                             (empty-seat new-layout x y)
                             (setf changed-p T)))
                                        ; No change - 
                          (T (setf (aref new-layout x y) (aref start-seats x y))))))
    (values new-layout changed-p)))


(defun seat-valid-p (seats coords)
  (and (<= 0 (first coords) (1- (first (array-dimensions seats))))
       (<= 0 (second coords) (1- (second (array-dimensions seats))))))

(defun sightline-check (seats seat direction)
  (let ((new-loc (mapcar #'+ seat direction)))
    (if (seat-valid-p seats new-loc)
        (cond ((occupied-p seats (first new-loc) (second new-loc)) #\O)
              ((empty-p seats (first new-loc) (second new-loc)) #\F)
              (T (sightline-check seats new-loc direction)))
        nil)))

(defun count-visible-seats (seats seat-x seat-y)
  (let ((seat-status (loop :for direction in '((0 1) (1 0) (-1 0) (0 -1) (-1 -1) (-1 1) (1 -1) (1 1))
                           collect (sightline-check seats (list seat-x seat-y) direction))))
    (count-if (lambda (c) (equal c #\O)) seat-status)))


(defun seat-change2 (start-seats)
  (let ((new-layout (make-array (array-dimensions start-seats)))
        (changed-p nil))
    (loop :for x below (first (array-dimensions start-seats))
          :do (loop :for y :below (second (array-dimensions start-seats))
                    :do (cond
                          ((and (empty-p start-seats x y)
                                (= (count-visible-seats start-seats x y) 0))
                           (progn
                             (occupy-seat new-layout x y)
                             (setf changed-p T)))
                          ((and (occupied-p start-seats x y)
                                (>= (count-visible-seats start-seats x y) 5))
                           (progn
                             (empty-seat new-layout x y)
                             (setf changed-p T)))
                          (T (setf (aref new-layout x y) (aref start-seats x y))))))
    (values new-layout changed-p)))


(defun count-all-seats (seats)
  (count-if-not #'null (loop :for x :below (first (array-dimensions seats))
                              append (loop :for y :below (second (array-dimensions seats))
                                           collect (occupied-p seats x y)))))

(defun day11/part1 (seat-layout)
  (multiple-value-bind (seats changed-p)
      (seat-change1 seat-layout)
    (if changed-p
        (day11/part1 seats)
        (count-all-seats seats))))


(defun day11/test1 ()
  (day11/part1 (parse-layout day11-test-input)))

(defun day11/solution1 ()
  (day11/part1 (parse-layout day11-input)))

(defun day11/part2 (seat-layout)
  (multiple-value-bind (seats changed-p)
      (seat-change2 seat-layout)
    (if changed-p
        (day11/part2 seats)
        (count-all-seats seats))))

(defun day11/test2 ()
  (day11/part2 (parse-layout day11-test-input)))

(defun day11/solution2 ()
  (day11/part2 (parse-layout day11-input)))
