(defvar *stack* (list))
(defvar *compile-mode* nil)
(defvar *in-string* nil)
(defvar *string-buffer* "")
(defvar *compile-buffer* "")

(defun split-by-one-space (string)
    "Returns a list of substrings of string
    divided by ONE space each.
    Note: Two consecutive spaces will be seen as
    if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))


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
	
;; Pop the top value of the stack and print it out
(defun print-and-pop-stack ()
  (format t "~a~%" (first (last *stack*)))
  (pop-stack))

(defun clear-screen ()
  (screen:with-window (screen:clear-window screen:*window*)))

;; Start compile mode (when defining words)
(defun start-compile ()
  (compile-mode t))
(defun end-compile ()
  (compile-mode nil))


;; The list containing all the words
(defvar *words* (list (list '.s 'print-stack)
					  (list '+ 'add-stack)
					  (list '- 'sub-stack)
					  (list '/ 'div-stack)
					  (list '* 'mul-stack)
					  (list 'drop 'pop-stack)
					  (list '\. 'print-and-pop-stack)
					  (list 'page 'clear-screen)
					  (list 'page 'clear-screen)
					  (list '\: 'start-compile)
					  (list 'concat 'concat-stack)
					  (list 'swap 'swap-stack)
					  (list 'dup 'dup-stack)
					  (list 'bye 'exit)))

;; Returns nil if has quote and returns t if not, (this inverse nature is so that (not (every ... )) will work on it
(defun has-quote-p (c)
	(if (char= #\" c) nil t))
(defun str-has-quote-p (str)
  (not (every #'has-quote-p str)))

(defun string-mode-eval (str)
	(setf *string-buffer* (concatenate 'string *string-buffer* str " "))
	(if (str-has-quote-p str)
	  (progn
		(setf *in-string* nil)
		;; Append value to stack, but cut out the last 2 chars of the string since they are an extra space and a quotation mark
		(setf *stack* (append *stack* (list (subseq *string-buffer* 0 (- (length *string-buffer*) 2)))))
		(setf *string-buffer* ""))))

(defun eval-token (token)
  (if (every #'digit-char-p token)
	(setf *stack* (append *stack* (list (parse-integer token))))
	(let ((val (assoc (intern token) *words*)))
	  (if val
		;; Check whether should pass args or not
		(if (> (length val) 2)
		  (funcall (cadr val) (caddr val))
		  (funcall (cadr val)))
		(format t "INVALID SYNTAX~%")))))

(defun eval-line (line)
  (let ((tokens (split-by-one-space line)))
	(dolist (token tokens)
	  (if (not *in-string*)
		(if (not (string= token ""))
		  (if (char= (char token 0) #\")
			(progn 
			  (setq *in-string* t) 
			  (string-mode-eval (subseq token 1 (length token))))
			(eval-token (string-upcase token))))
		(string-mode-eval token)))))

(loop
  (eval-line (read-line)))
