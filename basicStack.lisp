(defun pop-stack ()
  (let ((last-value (first (last *stack*))))
	(setf *stack* (butlast *stack*))
	last-value))

;; TODO Stack functions properly
(defun print-stack ()
  (format t "------ STACK STARTS ------ ~%")
  (dolist (item *stack*)
	(if (stringp item)
	  (format t "\"~a\"~%" item)
	  (format t "~a~%" item)))
  (format t "------- STACK ENDS ------- ~%"))

;; Pop the top value of the stack and print it out
(defun print-and-pop-stack ()
  (format t "~a~%" (first (last *stack*)))
  (pop-stack))

;; Some basic math operators for forth
(defun add-stack ()
  (let ((top (pop-stack)) (second-top (car (last *stack*))))
	(setf (nth (- (length *stack*) 1) *stack*) (+ second-top top))))
(defun sub-stack ()
  (let ((top (pop-stack)) (second-top (car (last *stack*))))
	(setf (nth (- (length *stack*) 1) *stack*) (- second-top top))))
(defun mul-stack ()
  (let ((top (pop-stack)) (second-top (car (last *stack*))))
	(setf (nth (- (length *stack*) 1) *stack*) (* second-top top))))
(defun div-stack ()
  (let ((top (pop-stack)) (second-top (car (last *stack*))))
	(setf (nth (- (length *stack*) 1) *stack*) (/ second-top top))))

;; Some basic string and vector functions
;; Concat stack is to be used with either of the stack orders:
;; String, string
;; Char, string
;; String, char
(defun concat-stack ()
  (let ((top (pop-stack)) (second-top (car (last *stack*))))
	(if (stringp top)
	  (if (stringp second-top)
		(setf (nth (- (length *stack*) 1) *stack*) (concatenate 'string second-top top))
		(setf (nth (- (length *stack*) 1) *stack*)
			  (coerce (append (list (code-char second-top)) (coerce top 'list)) 'string)))
	  ;; Add char to string
	  (setf (nth (- (length *stack*) 1) *stack*)
			(coerce (append (coerce second-top 'list) (list (code-char top))) 'string)))))

;; Some basic forth functions
(defun swap-stack ()
  (let ((top (first (last *stack*))))
	;; Assign last value on stack to second last
	(setf (nth (- (length *stack*) 1) *stack*) (car (last (butlast *stack*))))
	;; Assign second last value on stack to last
	(setf (nth (- (length *stack*) 2) *stack*) top)))

(defun dup-stack ()
  (setf *stack* (append *stack* (list (car (last *stack*))))))

(defun stack-to-string ()
	(setf (nth (- (length *stack*) 1) *stack*) (write-to-string (first (last *stack*)))))


;; If the stack starts out as abc the stack should output as bca
(defun rot-stack ()
  (let ((top (first (last *stack*))) (second-top (first (last (butlast *stack*)))))
	;; Assign the top stack value to the third from top stack value, using the example above this is assigning the top stack value to a
	(setf (nth (- (length *stack*) 1) *stack*) (car (last (butlast (butlast *stack*)))))
	(setf (nth (- (length *stack*) 2) *stack*) top)
	(setf (nth (- (length *stack*) 3) *stack*) second-top)))

;; If the stack starts out as abcd the stack should output as cdab
(defun two-swap-stack ()
  (let ((top (first (last *stack*))) (second-top (first (last (butlast *stack*))))
		(third-top (nth (- (length *stack*) 3) *stack*)))
	;; Assign the top stack value to the third from top stack value, using the example above this is assigning the top stack value to a
	(setf (nth (- (length *stack*) 1) *stack*) third-top)
	(setf (nth (- (length *stack*) 2) *stack*) (nth (- (length *stack*) 4) *stack*))
	(setf (nth (- (length *stack*) 3) *stack*) top)
	(setf (nth (- (length *stack*) 4) *stack*) second-top)))

(defun clear-screen ()
  ;; Use 2 different commands that both work on different types of terminals
;;  (screen:with-window (screen:clear-window screen:*window*))
;;  (screen:clear-window (screen:make-window))
  (format t "clearing....~%"))

(defun shell-stack ()
  (run-shell-command (pop-stack)))
