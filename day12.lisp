(in-package :advent2020)

(defparameter day12-input "~/Projects/advent-of-code-2020/input/day12-input.txt")
(defparameter day12-test-input "~/Projects/advent-of-code-2020/input/day12-test-input.txt")

(defparameter +cardinals+ '((north 0) (east 90) (south 180) (west 270)))

(defparameter *part1-actions* '((N . n1)
                                (S . S1)
                                (E . E1)
                                (W . W1)
                                (L . L1)
                                (R . R1)
                                (F . F1)))


(defstruct ship
  (course 'east)
  (position (list 0 0))
  (waypoint (list 10 1)))

(defun action-or-error1 (action-str)
  (let ((action (assoc action-str *part1-actions* :test #'string-equal)))
    (unless action
      (error "unknown action ~A" action-str))
    (cdr action)))

(defun parse-command (command)
  (list (action-or-error1 (subseq command 0 1))
        (parse-integer (subseq command 1))))

(defun move (boat val &optional (direction (ship-course boat)))
  (ecase direction
    (north (setf (cadr (ship-position boat)) (+ (cadr (ship-position boat)) val)))
    (east (setf (car (ship-position boat)) (+ (car (ship-position boat)) val)))
    (south (setf (cadr (ship-position boat)) (- (cadr (ship-position boat)) val)))
    (west (setf (car (ship-position boat)) (- (car (ship-position boat)) val)))))

(defun F1 (boat val)
  (move boat val))

(defun N1 (boat val)
  (move boat val 'north))

(defun E1 (boat val)
  (move boat val 'east))

(defun S1 (boat val)
  (move boat val 'south))

(defun W1 (boat val)
  (move boat val 'west))

(defun R1 (boat val)
  (let* ((course (cadr (find (ship-course boat) +cardinals+ :key #'car)))
         (new-course (mod (+ course val) 360)))
    (setf (ship-course boat)
          (car (find new-course +cardinals+ :key #'cadr)))))

(defun L1 (boat val)
  (R1 boat (- val)))

(defun manhattan-distance (boat)
  (+ (abs (car (ship-position boat)))
     (abs (cadr (ship-position boat)))))

(defun day12/part1 (file)
  (let ((ferry (make-ship))
        (commands (mapcar #'parse-command (uiop:read-file-lines file))))
    (loop :for command in commands
          do (funcall (car command) ferry (cadr command)))
    (manhattan-distance ferry)))

(defun day12/test1 ()
  (day12/part1 day12-test-input))

(defun day12/solution1 ()
  (day12/part1 day12-input))

(defparameter *part2-actions* '((N . N2)
                                (S . S2)
                                (E . E2)
                                (W . W2)
                                (L . L2)
                                (R . R2)
                                (F . F2)))

(defun action-or-error2 (action-str)
  (let ((action (assoc action-str *part2-actions* :test #'string-equal)))
    (unless action
      (error "unknown action ~A" action-str))
    (cdr action)))

(defun parse-command2 (command)
    (list (action-or-error2 (subseq command 0 1))
          (parse-integer (subseq command 1))))

;; Waypoint moving functions
(defun N2 (boat val)
  (setf (cadr (ship-waypoint boat)) (+ (cadr (ship-waypoint boat)) val)))

(defun S2 (boat val)
  (N2 boat (- val)))

(defun E2 (boat val)
  (setf (car (ship-waypoint boat)) (+ (car (ship-waypoint boat)) val)))

(defun W2 (boat val)
  (E2 boat (- val)))

;; Rotation of vector:
;;   x2 = sin(a).x1 - cos(a).y1
;;   y2 = cos(a).y1 + sin(a).x1

(defun dsin (angle)
  (sin (* (* 2 pi) (/ angle 360))))

(defun dcos (angle)
  (cos (* (/ angle 360) (* 2.0 pi))))

(defparameter +mult+ '((-270 1 0)
                       (-180 0 -1)
                       (-90 -1 0)
                       (0 0 1)
                       (90 1 0)
                       (180 0 -1)
                       (270 -1 0)))
(defun L2 (boat angle)
  (let ((sin-a (cadr (assoc angle +mult+)))
        (cos-a (caddr (assoc angle +mult+)))
        (x1 (car (ship-waypoint boat)))
        (y1 (cadr (ship-waypoint boat))))
    (setf (car (ship-waypoint boat)) (- (* cos-a x1) (* sin-a y1)))
    (setf (cadr (ship-waypoint boat)) (+ (* sin-a x1) (* cos-a y1)))))

(defun R2 (boat angle)
  (L2 boat (- angle)))

;; Move ship to waypoint n times
(defun F2 (boat val)
  (setf (ship-position boat)
        (mapcar #'+
                (ship-position boat)
                (mapcar (lambda (x)
                          (* val x))
                        (ship-waypoint boat)))))


(defun day12/part2 (file)
  (let ((ferry (make-ship))
        (commands (mapcar #'parse-command2 (uiop:read-file-lines file))))
    (loop :for command in commands
          do (funcall (car command) ferry (cadr command)))
    (manhattan-distance ferry)))

(defun day12/test2 ()
  (day12/part2 day12-test-input))

(defun day12/solution2 ()
  (day12/part2 day12-input))
