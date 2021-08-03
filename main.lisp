(defvar *stack* (list))
(defvar *compile-mode* nil)
(defvar *in-string* nil)
(defvar *string-buffer* "")
(defvar *compile-buffer* (list))

(defvar *words*)

(load "utils.lisp")
(load "basicStack.lisp")

(defun string-mode-eval (str)
	(setf *string-buffer* (concatenate 'string *string-buffer* str " "))
	(if (str-has-quote-p str)
	  (progn
		(setf *in-string* nil)
		;; Append value to stack, but cut out the last 2 chars of the string since they are an extra space and a quotation mark
		(setf *stack* (append *stack* (list (subseq *string-buffer* 0 (- (length *string-buffer*) 2)))))
		(setf *string-buffer* ""))))


(defun eval-words (lst)
  (dolist (item lst)
	(manage-token item)))

;; Start compile mode (when defining words)
(defun start-compile ()
  (setf *compile-mode* t))
;; End compile mode
(defun end-compile ()
  (if (> (length *compile-buffer*) 1)
	  (push (list (
				   intern (string-upcase (car *compile-buffer*)))
				  'eval-words
				  (cdr *compile-buffer*))
			*words*)
	  (format t "INVALID WORD, NOT COMPLETE~%"))
  (setf *compile-buffer* (list))
  (setf *compile-mode* nil))

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

(defun manage-token (token)
  (if (not *compile-mode*)
	  (if (not *in-string*)
		  (if (not (string= token ""))
			  (if (char= (char token 0) #\")
				  (progn 
					(setq *in-string* t) 
					(string-mode-eval (subseq token 1 (length token))))
				  (eval-token (string-upcase token))))
		  (string-mode-eval token))
	  (if (string= token ";")
		  (end-compile)	
		  (setf *compile-buffer* (append *compile-buffer* (list token))))))

(defun eval-line (line)
  (let ((tokens (split-by-one-space line)))
	(dolist (token tokens)
	  (manage-token token))))

;; The list containing all the words
(setf *words* (list (list '.s 'print-stack)
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
					(list '2swap 'two-swap-stack)					  
					(list 'rot 'rot-stack)
					(list 'dup 'dup-stack)
					(list 'bye 'exit)))

(loop
  (eval-line (read-line)))
