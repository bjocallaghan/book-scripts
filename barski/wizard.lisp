;; state-independent helper functions

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defun describe-path (edge)
  `(there is a ,(third edge) going ,(second edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun objects-at (location objects object-locations)
  (flet ((object-at-p (object)
           (eq location (cadr (assoc object object-locations)))))
    (remove-if-not #'object-at-p objects)))

(defun describe-objects (location objects object-locations)
  (labels ((describe-obj (object)
             `(you see a ,object on the floor.)))
    (apply #'append
           (mapcar #'describe-obj
                   (objects-at location objects object-locations)))))

;;; functions that use global state

(defun look ()
  (declare (special *location* *nodes* *edges* *objects* *object-locations*))
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (declare (special *location* *edges*))
  (let ((next (find direction
                    (cdr (assoc *location* *edges*)) :key #'cadr)))
    (if next
        (progn (setf *location* (car next)) (look))
        '(you cannot go that way))))

(defun pickup (object)
  (declare (special *location* *objects* *object-locations*))
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))

(defun inventory ()
  (declare (special *objects* *object-locations*))
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;;; setup the game

(defparameter *nodes* '((living-room (you are in the living room. a wizard is
                                      snoring loudly on the couch.))
                        (garden (you are in a beautiful garden. there is a well
                                 in front of you.))
                        (attic (you are in the attic. there is a giant welding
                                torch in the corner.))))

(defparameter *edges* '((living-room
                         (garden west door)
                         (attic upstairs ladder))
                        (garden
                         (living-room east door))
                        (attic
                         (living-room downstairs ladder))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

(defparameter *starting-location* 'living-room)
(defparameter *location* *starting-location*)

;;; interface

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
             (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i "don't" know that command.)))

(defun game-print (sexp)
  (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string sexp))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))

(defun tweak-text (list caps lit)
  (when list
    (let ((item (car list))
          (rest (cdr list)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eq item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            ((or caps lit) (cons (char-upcase item) (tweak-text rest nil nil)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))
