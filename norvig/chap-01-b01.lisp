(defparameter *p* '(John Q Public))
(defparameter *town* (list 'Anytown 'USA))

(defparameter *names* '((John Q Public) (Admiral Grace Murray Hopper) (Spot)
                        (Aristotle) (A A Milne) (Z Z Top) (Sir Larry Olivier)
                        (Miss Scarlet)))
(defparameter *titles* '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name.")

(defun first-name (name)
  "Select the first name from a name represented as a list."
  (first name))
(defun first-name (name)
  "Select the first name from a name represented as a list."
  (if (find (first name) *titles*)
      (first-name (cdr name))
      (first name)))

(defun last-name (name)
  "Select the last name from a name represented as a list."
  (car (last name)))

(defun mappend (fn lst)
  "Apply fn to each element of list and append the results"
  (apply #'append (mapcar fn lst)))

(defun self-and-double (x) (list x (+ x x)))

(mappend #'(lambda (x) (list x (- x))) '(1 2 3))
