(in-package :day20)

(defparameter day20-test-input "~/Projects/advent-of-code-2020/input/day20-test-input.txt")
(defparameter day20-input "~/Projects/advent-of-code-2020/input/day20-input.txt")

;; Parse tiles from input string
;; - First line: Tile nnnn:



(defun test1 ()
  (parse-input day20-test-input))

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

;;------------------------------------------------------------------------------
;; Puzzle input:
;;  144 tiles, making a picture 12x12. Each tile has 10x10 array.
;;------------------------------------------------------------------------------

;(defun tile-array )

;; Rotation changes the value of the number, unless all numbers are read from the same end.
;; Need to change tile-boundaries to consider this point. Otherwise simple comparison
;; of the numbers is not possible.

;; (defun build-picture (picture tiles next-pos)
;;   (if (match-picture tiles next-pos)
;;       ))


;; Parameters to our recursive routine
;; - picture - current assembled picture. Initially empty. This is a list of lists for each row
;; - tiles - tiles not yet allocated into the picture
;;
;; Steps
;; - Is the tile list empty? If so, then calculate and return the result - picture is complete
;; - for each tile in tile list
;;     check for a match in each of the orientations
;;       - If found
;;         Add the tile to the picture (in the appropriate orientation)
;;         Recursively call the routine, with the new picture and tile list
;;         if the result is non-zero (ie - the solution was found)
;;            return the result
;;
;;   return 0
;;

(defun tile-fit (picture next-pos tile))

(defun add-to-picture (picture next-pos tile))

(defun calculate-result (picture))

;; Return the tile id
(defun tile-id (tile)
  (car tile))

(defun build-picture picture tiles next-pos
  (if ((length tiles) 0)
      (calculate-result picture)
      (loop :for tile :in tiles
            :do (loop :for rot-tile in (tile-rotations tile)
                      :do (if (tile-fit picture next-pos rot-tile)
                              (let ((result (build-picture (add-to-picture picture next-pos rot-tile))))
                                (if (not (equal result 0))
                                    )
                               0
                                  0
                                  )))
                
                ;; for each tile-rotation
                ;; Check to see if the tile fits the picture at the next-possible
                ;; if it does,
                ;;     Add the tile to the picture
                ;;     Remove the tile from the list of tiles
                ;;     call build-picture with the new picture, tiles and new next-possible
                ;;
            )
      ))


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
    (list (car tile) (list (edge-rotate (cadddr edges))
                           (car edges)
                           (edge-rotate (cadr edges))
                           (caddr edges)))))

(defun tile-rotations (tile)
  (let* ((1st-rotation (tile-rotate tile))
         (2nd-rotation (tile-rotate 1st-rotation))
         (3rd-rotation (tile-rotate 2nd-rotation)))
    (list tile 1st-rotation 2nd-rotation 3rd-rotation)))


