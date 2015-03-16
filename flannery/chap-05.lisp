(defun make-factor-table (upper-limit &optional verbose)
  (when verbose (print "making a factor table..."))
  (let ((table (make-array (1+ upper-limit)
                           :initial-element 0
                           :element-type `(integer 0 ,upper-limit))))
    (setf (elt table 0) 1)
    (setf (elt table 1) 1)
    (do ((n 2 (1+ n)))
        ((> n upper-limit))
      (when (zerop (elt table n))
        (do ((m (+ n n) (+ m n)))
            ((> m upper-limit))
          (setf (elt table m) n))))
    (when verbose (print "finished"))
    table))

(defparameter *factor-table* (make-factor-table 50000000 t))

(defun prime-factorization (n)
  "Returns a list of the prime factors of N."
  ;; not generalized. error when go out of bounds of *factor-table*
  (unless (> n 1) (error "N must be an integer larger than 1."))
  (labels ((p-fact (n acc)
             (let ((d (elt *factor-table* n)))
               (if (zerop d)
                   (cons n acc)
                   (cons d (p-fact (/ n d) acc))))))
    (nreverse (p-fact n nil))))

(defun primep (n)
  (zerop (elt *factor-table* n)))

(defun factors (n)
  "Return a list of the factors of a N."
  ;; mid-point junk ensures no duplicate factors for perfect squares; efficient
  (let (lower-factors upper-factors (mid-point (isqrt (1- n))))
    (loop for d from 1 to mid-point when (zerop (mod n d))
       do (progn (push d lower-factors) (push (/ n d) upper-factors)))
    (nconc (nreverse lower-factors)
           (when (= n (* (1+ mid-point) (1+ mid-point))) (list (1+ mid-point)))
           upper-factors)))

(defun hilbert (n)
  "Returns the Nth Hilbert number."
  (unless (plusp n) (error "N must be a positive integer."))
  (1+ (* 4 (1- n))))

(defun hilbertp (n)
  "Returns t if N is a Hilbert number, otherwise nil."
  (when (plusp n)
    (zerop (mod (1- n) 4))))

(defun hilbert-prime-p (n)
  "Returns t if N is a Hilbert prime, otherwise nil."
  (when (and (> n 1)
             (hilbertp n))
    (loop for h from 5 to (isqrt n) by 4 never (hilbertp (/ n h)))))

;; todo: make recursive to find all possibilities, not just pairs
(defun hilbert-prime-factorizations (n)
  "Returns a list of possible Hilbert prime factorizations for N. Since a given
number is not guaranteed to have a unique Hilbert prime factorization, multiple
results are allowed."
  (let (factorizations)
    (do ((h 5 (+ h 4)))
        ((> h (isqrt n)) (nreverse factorizations))
      (when (hilbert-prime-p h)
        (let ((q (/ n h)))
          (when (hilbertp q) (push (list h q) factorizations)))))))

(defun fermat (n)
  "Returns the Nth Fermat number."
  (1+ (expt 2 (expt 2 n))))
