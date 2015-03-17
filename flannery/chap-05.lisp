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

(defun euler (n)
  "Returns the Nth Euler number."
  (+ 41 (- n) (* n n)))

(loop for i from 1 to 50
   do (format t "E(~2d) = ~6d = ~a~%"
              i (euler i) (factor:prime-factorization (euler i))))

(defun mersenne (n)
  "Returns the Nth Mersenne number."
  (1- (expt 2 n)))

(let ()
  (loop for i from 2 to 70 when (factor:primep i)
     do
       (format t "~%  attempting ~d" i)
       (let ((factors (factor:prime-factorization (mersenne i))))
         (when (= (length factors) 1)
           (format t "~%E(~2d) = ~20d = ~a"
                   i (mersenne i) (factor:prime-factorization (mersenne i))))))
  (format t "~%DONE.~%"))
