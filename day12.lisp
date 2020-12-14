(in-package :advent2020)

(defparameter day12-input "~/Projects/advent-of-code-2020/input/day12-input.txt")
(defparameter day12-test-input "~/Projects/advent-of-code-2020/input/day12-test-input.txt")

(defparameter +cardinals+ '((north 0) (east 90) (south 180) (west 270)))

(defparameter *actions* '(N S E W L R F))

(defstruct ship
  (course 'east)
  (position (list 0 0)))

(defun action-or-error (action-str)
  (let ((action (find action-str *actions* :test #'string-equal)))
    (unless action (error "unknown action ~A" action-str))
    action))

(defun parse-command (command)
  (list (action-or-error (subseq command 0 1))
        (parse-integer (subseq command 1))))

(defun move (boat val &optional (direction (ship-course boat)))
  (ecase direction
    (north (setf (cadr (ship-position boat)) (+ (cadr (ship-position boat)) val)))
    (east (setf (car (ship-position boat)) (+ (car (ship-position boat)) val)))
    (south (setf (cadr (ship-position boat)) (- (cadr (ship-position boat)) val)))
    (west (setf (car (ship-position boat)) (- (car (ship-position boat)) val)))))

(defun F (boat val)
  (move boat val))

(defun N (boat val)
  (move boat val 'north))

(defun E (boat val)
  (move boat val 'east))

(defun S (boat val)
  (move boat val 'south))

(defun W (boat val)
  (move boat val 'west))

(defun R (boat val)
  (let* ((course (cadr (find (ship-course boat) +cardinals+ :key #'car)))
         (new-course (mod (+ course val) 360)))
    (setf (ship-course boat)
          (car (find new-course +cardinals+ :key #'cadr)))))

(defun L (boat val)
  (R boat (- val)))

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

