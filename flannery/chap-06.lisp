(defun mod-age (age)
  (list (mod age 3) (mod age 5) (mod age 7)))

;; where in blazes does this formula come from?
(defun reconstitute-age (mod-triplet)
  (mod (+ (* 70 (first  mod-triplet))  ; 70 = 5 * 7 ... * 2 (!) why 2?
          (* 21 (second mod-triplet))  ; 21 = 3 * 7
          (* 15 (third  mod-triplet))) ; 15 = 3 * 5
       105))

(loop for age from 0 to 105
   do (let ((r-age (reconstitute-age (mod-age age))))
        (format t "~&~3d --> ~a --> ~3d ~a~%"
                age (mod-age age) r-age (= age r-age))))

(loop for age from 0 to 105 by 7
   do (let ((r-age (reconstitute-age (mod-age age))))
        (format t "~&~3d --> ~a --> ~3d ~a~%"
                age (mod-age age) r-age (= age r-age))))

(defun caesar-cipher (n message)
  (let* ((n (mod n 26))
         (alpha (coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'list))
         (shifted (copy-seq alpha)))
    (setf (cdr (last shifted)) shifted) ; make circular
    (setf shifted (nthcdr n shifted)) ; do the shift
    (let (result)
      (loop for char in (coerce (write-to-string message) 'list)
         do (let ((index (position (char-upcase char) alpha)))
              (when index (push (elt shifted index) result))))
      (coerce (nreverse result) 'string))))

(defun caesar-encode (n message)
  (caesar-cipher n message))

(defun caesar-decode (n message)
  (caesar-cipher (- n) message))

(defparameter *message-1* '(this message is top secret))
(defparameter *encoded-1* '(wklvlvkrzzhghflskhu))
(defparameter *message-2* '(thisishowwedecipher))
(defparameter *encoded-2* '(wubdjdlq))
(defparameter *message-3* '(cold))

(format t "~2&~a~%" (caesar-encode 4 "pecan"))
(format t "~2&~a~%" (caesar-encode 9 "sleep"))
