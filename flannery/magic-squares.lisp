(defun range (n)
  "Returns a list with elements 0 to N-1."
  (let (range) (dotimes (i n (nreverse range)) (push i range))))

(defun magic-square-p (square &optional (magic-sum 15))
  "Returns T if the provided SQUARE is a magic square."
  (and (loop for row in (rows square)
          always (= magic-sum (sum row)))
       (loop for column in (columns square)
          always (= magic-sum (sum column)))
       (loop for diagonal in (diagonals square)
          always (= magic-sum (sum diagonal)))))

(defun sum (seq)
  "Returns the numeric sum of a sequence."
  (reduce #'+ seq))

;;; matrix operation stuff -- can factor out later

(defun row (array row-index)
  "Returns the row of ARRAY at index ROW-INDEX as a vector."
  (map 'vector
       #'(lambda (x) (aref array row-index x))
       (range (array-dimension array 1))))

(defun col (array col-index)
  "Returns the column of ARRAY at index COL-INDEX as a vector."
  (map 'vector
       #'(lambda (x) (aref array x col-index))
       (range (array-dimension array 0))))

(defun rows (array)
  "Retuns the rows of a 2d ARRAY as a sequence of vectors."
  (let (rows
        (num-rows (array-dimension array 0))
        (num-cols (array-dimension array 1)))
    (dotimes (row-index num-rows (nreverse rows))
      (let ((row-vec (make-array num-cols)))
        (dotimes (col-index num-cols)
          (setf (aref row-vec col-index) (aref array row-index col-index)))
        (push row-vec rows)))))

(defun columns (array)
  "Retuns the rows of a 2d ARRAY as a sequence of vectors."
  (let (cols
        (num-rows (array-dimension array 0))
        (num-cols (array-dimension array 1)))
    (dotimes (col-index num-cols (nreverse cols))
      (let ((col-vec (make-array num-rows)))
        (dotimes (row-index num-rows)
          (setf (aref col-vec row-index) (aref array row-index col-index)))
        (push col-vec cols)))))

(defun diagonals (array)
  "Retuns the diagonals of a 2d square ARRAY as a sequence of two vectors."
  (let ((num-rows (array-dimension array 0))
        (num-cols (array-dimension array 1)))
    (unless (= num-rows num-cols) (error "can only do diagonals of a square"))
    (let ((diag-1 (make-array num-rows))
          (diag-2 (make-array num-rows)))
      (dotimes (row-index num-rows)
        (setf (aref diag-1 row-index)
              (aref array row-index row-index))
        (setf (aref diag-2 row-index)
              (aref array row-index (- num-rows row-index 1))))
      (list diag-1 diag-2))))

(defun copy-square (square)
  (make-array (list (array-dimension square 0)
                    (array-dimension square 1))
              :initial-contents (rows square)))

(defun rotate-square (square)
  "Returns a clockwise-rotated square based on a 2d array SQUARE."
  (let ((new-rows (mapcar #'(lambda (col) (nreverse col)) (columns square))))
    (make-array (list (array-dimension square 0)
                      (array-dimension square 1))
                :initial-contents new-rows)))

(defun mirror-square (square &key (axis :horizontal))
  "Returns a mirror image of SQUARE rotated around a vertical axis."
  (ecase axis
    (:horizontal
     (make-array (list (array-dimension square 0)
                       (array-dimension square 1))
                 :initial-contents (mapcar #'nreverse (rows square))))
    (:vertical
     (make-array (list (array-dimension square 0)
                       (array-dimension square 1))
                 :initial-contents (nreverse (rows square))))
    (:diagonal
     (mirror-square
      (rotate-square
       (make-array (list (array-dimension square 0)
                         (array-dimension square 1))
                   :initial-contents (rows square)))
      :axis :horizontal))))

;;; normalize-square is sort of a mess -- i think it's correct for 3x3 though
(defun normalize-square (square)
  "Returns the normalized version of SQUARE.

Normalized is when the lowest corner digit is in the upper-right, with the next
lowest in the mid-upper. If no corners are set, mid-upper is key one, mid-left
is key two."
  (let ((norm-square (copy-square square))
        (num-rows (array-dimension square 0))
        (num-cols (array-dimension square 1)))
    ;; todo: generalize the function
    (unless (and (= 3 num-rows) (= 3 num-cols)) (error "limit 3x3 for now"))
    (let ((corners (list (aref square 0 0)
                         (aref square 2 0)
                         (aref square 2 2)
                         (aref square 0 2)))
          (middles (list (aref square 0 1)
                         (aref square 1 2)
                         (aref square 2 1)
                         (aref square 1 0))))
      (flet ((lowest-position (sequence)
               (position (apply #'min (remove-if #'zerop sequence)) sequence))
             (needs-mirror (square)
               (cond
                 ((or (plusp (aref square 0 1))
                      (plusp (aref square 1 0)))
                  (or (zerop (aref square 0 1))
                      (and (plusp (aref square 1 0))
                           (> (aref square 0 1) (aref square 1 0)))))
                 ((or (plusp (aref square 0 2))
                      (plusp (aref square 2 0)))
                  (or (zerop (aref square 0 2))
                      (and (plusp (aref square 2 0))
                           (> (aref square 0 2) (aref square 2 0)))))
                 ((or (plusp (aref square 1 2))
                      (plusp (aref square 2 1)))
                  (or (zerop (aref square 1 2))
                      (and (plusp (aref square 2 1))
                           (> (aref square 1 2) (aref square 2 1))))))))
        (if (remove-if #'zerop corners)
            (dotimes (i (lowest-position corners))
              (setf norm-square (rotate-square norm-square)))
            (when (remove-if #'zerop middles)
              (dotimes (i (lowest-position middles))
                (setf norm-square (rotate-square norm-square)))))
        (when (needs-mirror norm-square)
          (setf norm-square (mirror-square norm-square :axis :diagonal))))
      norm-square)))

(defun square= (square-1 square-2)
  (string= (write-to-string square-1) (write-to-string square-2)))

(defun square-equiv (square-1 square-2)
  (square= (normalize-square square-1) (normalize-square square-2)))

(rt:deftest copy-square-test
    (let ((square #2a ((1 2 3)
                       (4 5 6)
                       (7 8 9))))
      (square= (copy-square square) square)) t)

(rt:deftest rotate-square-clockwise
    (let ((square-1 #2a ((1 2 3)
                         (4 5 6)
                         (7 8 9)))
          (square-2 #2a ((7 4 1)
                         (8 5 2)
                         (9 6 3))))
      (square= (rotate-square square-1) square-2)) t)

(rt:deftest mirror-square-horizontally
    (let ((square-1 #2a ((1 2 3)
                         (4 5 6)
                         (7 8 9)))
          (square-2 #2a ((3 2 1)
                         (6 5 4)
                         (9 8 7))))
      (square= (mirror-square square-1) square-2)) t)

(rt:deftest normalize-square-1
    (let ((square-1 #2a ((1 4 7)
                         (2 5 8)
                         (3 6 9)))
          (square-2 #2a ((1 2 3)
                         (4 5 6)
                         (7 8 9))))
      (square= (normalize-square square-1) square-2)) t)

(rt:deftest normalize-square-2
    (let ((square-1 #2a ((0 0 9)
                         (0 0 0)
                         (0 0 4)))
          (square-2 #2a ((4 0 9)
                         (0 0 0)
                         (0 0 0))))
      (square= (normalize-square square-1) square-2)) t)

(rt:deftest normalize-square-3
    (let ((square-1 #2a ((0 0 0)
                         (0 0 2)
                         (0 4 0)))
          (square-2 #2a ((0 2 0)
                         (4 0 0)
                         (0 0 0))))
      (square= (normalize-square square-1) square-2)) t)

(defparameter *perfect-square* #2a ((8 1 6)
                                    (3 5 7)
                                    (4 9 2)))
(defparameter *wrong-square* #2a ((5 6 4)
                                  (9 7 8)
                                  (1 2 3)))

;; actual magic squares tests (not just matrix junk)

(rt:deftest magic-square
    (magic-square-p *perfect-square*) t)

(rt:deftest non-magic-square
    (magic-square-p *wrong-square*) nil)

(rt:do-tests)

(defun sq (input)
  (let ((square (make-array '(3 3) :initial-element 0))
        (text (write-to-string input)))
    (dotimes (i 9 square)
      (setf (aref square (floor i 3) (mod i 3))
            (read-from-string (subseq text i (1+ i)))))))
