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

(defun fill-into-array (target-array num-list)
  "Fills in a 2d array with the numbers in num-list"
  (let ((nums (copy-seq num-list)))
    (dotimes (row-index (array-dimension target-array 0) target-array)
      (dotimes (col-index (array-dimension target-array 1))
        (setf (aref target-array row-index col-index) (pop nums))))))

;;; tests

(defparameter *perfect-square* (make-array '(3 3)))
(fill-into-array *perfect-square* '(8 1 6 3 5 7 4 9 2))
(defparameter *imperfect-square* (make-array '(3 3)))
(fill-into-array *imperfect-square* '(5 6 4 9 7 8 1 2 3))

(ql:quickload :rt)

(rt:deftest magic-square
    (magic-square-p *perfect-square*) t)

(rt:deftest non-magic-square
    (magic-square-p *imperfect-square*) nil)

(rt:do-tests)
