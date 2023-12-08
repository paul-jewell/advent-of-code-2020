(in-package :day20)

(defparameter day20-test-input "~/Projects/advent-of-code-2020/input/day20-test-input.txt")
(defparameter day20-input "~/Projects/advent-of-code-2020/input/day20-input.txt")

;; Parse tiles from input string
;; - First line: Tile nnnn:

(defun parse-input (file)
  (let* ((tile-strs (split "\\n\\n" (uiop:read-file-string file))))
    (mapcar #'tile-boundaries (mapcar (lambda (tile-str)
                                        (let* ((tile-data (split "\\n" tile-str))
                                               (tile-number (parse-integer (car tile-data) :start 5 :junk-allowed t))
                                               (tile-image (cdr tile-data)))
                                          (list tile-number tile-image))) 
                                      tile-strs))))

(defun numerate-string (str)
  (parse-integer
   (concatenate 'string
                (loop :for char :across str
                      :collect
                      (if (char= char #\#)
                          #\1
                          #\0))) :radix 2))


(defun tile-boundaries (tile)
  (let ((north (numerate-string (caadr tile)))
        (east (numerate-string
               (apply #'concatenate 'string
                      (loop :for line :in (cadr tile)
                            :collect
                            (subseq line 9)))))
        (south (apply #'numerate-string (list (car (last (cadr tile))))))
        (west (numerate-string
               (apply #'concatenate 'string
                      (loop :for line :in (cadr tile)
                            :collect
                            (subseq line 0 1))))))
    (list (car tile) (list north east south west))))

(defun north (tile)
  (first (cadr tile)))

(defun east (tile)
  (second (cadr tile)))

(defun south (tile)
  (third (cadr tile)))

(defun west (tile)
  (fourth (cadr tile)))

(defun tile-side (cardinal picture position)
  (funcall cardinal (aref picture (array-pos-col position picture) (array-pos-row position picture))))

(defun tile-fit-pred (picture next-pos tile)
  ;;(format T "picture: ~A, next-pos: ~A, tile: ~A~%" picture next-pos tile)
  (let ((position (array-pos next-pos picture)))
    (if (= (car position) 0) ;; First column
        (if  (= (cadr position) 0) ;; First cell - top left - automatically fit!
             T
             (if (= (north tile)
                    (tile-side #'south picture (1-  next-pos)))
                 T     ;; Tile fits
                 nil))
        (if (= (cadr position) 0) ;; Check only east/west match
            (if (= (west tile)
                   (tile-side #'east picture
                              (let ((pos  (- next-pos (car (array-dimensions picture)))))
                                (if  (>= pos 0)
                                     pos
                                     0))))
                T
                nil)
            (if (and (= (west tile)
                        (tile-side #'east picture
                                   (let ((pos (- next-pos (car (array-dimensions picture)))))
                                     (if (>= pos 0)
                                         pos
                                         0))))
                     (= (north tile)
                        (tile-side #'south picture (1- next-pos))))
                T
                nil)))))

(defun add-to-picture (picture next-pos tile)
  ;;(format T "Before adding tile: picture: ~A, next-pos: ~A, tile: ~A~%" picture next-pos tile)
  (setf (aref picture (array-pos-col next-pos picture) (array-pos-row next-pos picture)) tile)
  ;;(format T "After: picture: ~A, next-pos: ~A, tile: ~A~%" picture next-pos tile)
  picture)

(defun calculate-result (picture)
  (let* ((array-dims (array-dimensions picture))
         (max-row (1- (cadr array-dims)))
         (max-col (1- (car array-dims))))
    (* (car (aref picture 0 0))
       (car (aref picture max-col 0))
       (car (aref picture 0 max-row))
       (car (aref picture max-col max-row)))))

;; Return the tile id
(defun tile-id (tile)
  (car tile))

(defparameter tiles (parse-input day20-test-input))

;; (0,0) - top left hand corner
;; (x,y) - indexing down array then across... (significant for tile matching)
(defun array-pos-row (next-pos picture)
  (floor (/ next-pos (car (array-dimensions picture)))))

(defun array-pos-col (next-pos picture)
  (mod next-pos (car (array-dimensions picture))))

(defun array-pos (next-pos picture)
  (list
   (array-pos-col next-pos picture)
   (array-pos-row next-pos picture)))

;; input parameters:
;; - picture - array of tiles positioned
;; - tiles - list of remaining tiles to be added to the picture (unrotated)
;; - next-pos - (x y) - position to test for next tile

(defun build-picture (picture tiles next-pos)
  (if (= (length tiles) 0) ;; Picture is fully constructed - return result
      (calculate-result picture)
      (loop :for tile :in tiles ;; Need to locate the next tile
            :do (loop :for rot-tile in (tile-rotations tile) ;; step through the tile rotations
                      :do (if (tile-fit-pred picture next-pos rot-tile) ;; Check if the tile fits
                              (let ((result (build-picture (add-to-picture picture next-pos rot-tile)
                                                           (cdr tiles)
                                                           (1+ next-pos))))
                                (if (not (equal result 0))
                                    result
                                    0)))))))



(defun part1 (input-file)
  (let* ((input-tiles (parse-input input-file))
         (pic-size (round (sqrt (length tiles))))
         (picture (make-array (list pic-size pic-size)))
         (next-pos 0)
         (tile-list (rotate-list input-tiles)))
    (loop for tiles in tile-list
          collect (build-picture picture tiles next-pos))))



(defun test1 ()
  (part1 day20-test-input))

;;-------------------------------------------------------------------------------------------
;; These functions are working:

(defun edge-rotate (edge)
  (parse-integer (reverse
                  (format nil "~10,'0,,b" edge)) ;; leading zero pad to full width
                 :radix 2))

(defun tile-rotate (tile)
  ;; North to East - no change
  ;; East to South - rotation required
  ;; South to West - no change
  ;; West to North - rotation required
  (let ((edges (cadr tile)))
    (list (car tile) (list (edge-rotate (cadddr edges)) ;; W to N
                           (car edges)                  ;; N to E
                           (edge-rotate (cadr edges))   ;; E to S
                           (caddr edges)))))            ;; S to W

(defun tile-rotations (tile)
  "Perform tile rotation and return list of rotations"
  (let* ((1st-rotation (tile-rotate tile))
         (2nd-rotation (tile-rotate 1st-rotation))
         (3rd-rotation (tile-rotate 2nd-rotation))
         (flipped-tile (tile-flip tile))
         (flipped-tile-1st-rotation (tile-rotate flipped-tile))
         (flipped-tile-2nd-rotation (tile-rotate flipped-tile-1st-rotation))
         (flipped-tile-3rd-rotation (tile-rotate flipped-tile-2nd-rotation))
         )
    (list tile 1st-rotation 2nd-rotation 3rd-rotation
          flipped-tile flipped-tile-1st-rotation flipped-tile-2nd-rotation flipped-tile-3rd-rotation)))

(defun tile-flip (tile)
  (let ((edges (cadr tile)))
    (list (car tile) (list (car edges)
                           (cadddr edges)
                           (caddr edges)
                           (cadr edges)))))


(defun rotate-list (list)
  "Rotate a list returning a list of all the rotations possible"
  (let ((list-length (length list)))
    (loop for l below list-length
          collect (let ((element (nth  l list)))
                    (reverse  (append (remove element list :test #'equal) (cons element ())))))))

