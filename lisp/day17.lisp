(in-package #:day17)

(defparameter input "~/Projects/advent-of-code-2020/input/day17-input.txt")
(defparameter test-input "~/Projects/advent-of-code-2020/input/day17-test-input.txt")

;; Design choices
;; --------------
;; Initial array is 2 dimensional, and represents the layer in the middle of the
;; cube. Each iteration will grow the array in all 6 directions by 1. I could start
;; with an array sized to accommodate the full cube after the defined number of cycles
;; but optimise the calculations in each cycle to match the affected region in the cube.
;; For part 1, I don't expect this to be a signficant issue, but we will see what part
;; 2 brings.

;; TODO: Revisit logic for this - may be better to build the new array for each iteration
;;       during processing

(defun expand-layer (size initial-state)
  "Build and return a layer of inactive cells around the initial state."
  (let* ((initial-size (length initial-state))
         (side-padding (make-list size :initial-element #\.))
         (row-padding (make-list (+ (* 2 size) initial-size) :initial-element #\.)))
    (loop :for i :below (+ initial-size (* size 2))
          :if (or (< i size)
                  (>= i (+ size initial-size)))
            :append (list row-padding)
          :else
            :append (list (append side-padding
                                   (nth (- i size) initial-state)
                                   side-padding)))))

(defun build-3d-cube (size initial-state)
  (let* ((new-size (+ (* size 2) (length initial-state)))
         (layer-padding (loop :for i :below new-size
                              :append (list (make-list new-size :initial-element #\.)))))
    ;; looping through z axis. Initial state is two dimensional, so need to added
    ;; SIZE layers above and below the layer with the initial content.
    (loop :for i :below (+ (* size 2) (length (car initial-state)))
          :if (= i (ceiling (+ size (/ (length (car initial-state))))))
            :append (list (expand-layer size initial-state))
          :else
            :append (list layer-padding))))

(defun new-state (cube x y z)
  (let* ((state (aref cube x y z))
         (max-size (1- (car (array-dimensions cube))))
         (x-min (max (- x 1) 0))
         (x-max (min (+ x 1) max-size))
         (y-min (max (- y 1) 0))
         (y-max (min (+ y 1) max-size))
         (z-min (max (- z 1) 0))
         (z-max (min (+ z 1) max-size))
         (count-active (-  (loop :for x :from x-min :to x-max
                                 :sum (loop :for y :from y-min :to y-max
                                            :sum (loop :for z from z-min :to z-max
                                                       :count (char= (aref cube x y z) #\#))))
                           (if (char= state #\#) ; Subtract candidate from sum if active
                               1 0))))
    (if (char= state #\#)
        (if (<= 2 count-active 3) #\# #\.)       ; Already active - stay active if 2 or 3 around
        (if (= 3 count-active)    #\# #\.))))    ; Become active if exactly 3 

(defun process-3d-step (cube moves)
  (if (= 0 moves)
      cube
      (let* ((cube-size (car (array-dimensions cube)))
             (new-cube (make-array (array-dimensions cube)
                                   :initial-element #\.)))
        (loop :for x :below cube-size
              :do (loop :for y :below cube-size
                        :do (loop :for z :below cube-size
                                  :do (setf (aref new-cube x y z) (new-state cube x y z)))))
        (process-3d-step new-cube (1- moves)))))

(defun count-active (cube)
  (count #\# (make-array (array-total-size cube) :displaced-to cube)))

(defun part1 (file moves)
  (let* ((initial-state (mapcar (lambda (s) (coerce s 'list)) (uiop:read-file-lines file)))
         (expanded-data (build-3d-cube moves initial-state))
         (size (length (car expanded-data)))
         (cube (make-array (list size size size) :initial-contents expanded-data)))
    (let ((cube (process-3d-step cube moves)))
      (count-active cube))
    ))

(defun test1 ()
  (part1 test-input 6))

(defun solution1 ()
  (part1 input 6))

(defun build-4d-cube (size initial-state)
  (let* ((new-size (+ (* size 2) (length initial-state)))
         (layer-padding (make-list new-size
                                   :initial-element
                                   (make-list new-size :initial-element #\.))))
    (loop
      :for z :below new-size
      :append (list (loop
                       :for w below new-size
                       :if (and (= w size) (= z size))
                         :append (list (expand-layer size initial-state))
                       :else
                         :append (list layer-padding))))))

(defun new-4d-state (cube x y z w)
  (let* ((state (aref cube x y z w))
         (max-size (1- (car (array-dimensions cube))))
         (x-min (max (- x 1) 0))
         (x-max (min (+ x 1) max-size))
         (y-min (max (- y 1) 0))
         (y-max (min (+ y 1) max-size))
         (z-min (max (- z 1) 0))
         (z-max (min (+ z 1) max-size))
         (w-min (max (- w 1) 0))
         (w-max (min (+ w 1) max-size))
         (count-active (- (loop
                            :for x :from x-min :to x-max
                            :sum (loop
                                   :for y :from y-min :to y-max
                                   :sum (loop
                                          :for z :from z-min :to z-max
                                          :sum (loop :for w :from w-min :to w-max
                                                     :count (char= (aref cube x y z w) #\#)))))
                          (if (char= state #\#) ; Subtract candidate from sum if active
                              1 0))))
    (if (char= state #\#)
        (if (<= 2 count-active 3) #\# #\.)       ; Already active - stay active if 2 or 3 around
        (if (= 3 count-active)    #\# #\.))))    ; Become active if exactly 3 

(defun process-4d-step (cube moves)
  (if (= 0 moves)
      cube
      (let* ((cube-size (car (array-dimensions cube)))
             (new-cube (make-array (array-dimensions cube)
                                   :initial-element #\.)))
        (loop
          :for x :below cube-size
          :do (loop
                :for y :below cube-size
                :do (loop
                      :for z :below cube-size
                      :do (loop
                            :for w :below cube-size
                            :do (setf (aref new-cube x y z w) (new-4d-state cube x y z w))))))
        (process-4d-step new-cube (1- moves)))))

(defun part2 (file moves)
  (let* ((initial-state (mapcar (lambda (s) (coerce s 'list)) (uiop:read-file-lines file)))
         (expanded-data (build-4d-cube moves initial-state))
         (size (length (car expanded-data)))
         (cube (make-array (list size size size size) :initial-contents expanded-data)))
    (count-active (process-4d-step cube moves))))

(defun test2 ()
  (part2 test-input 6))

(defun solution2 ()
  (part2 input 6))


