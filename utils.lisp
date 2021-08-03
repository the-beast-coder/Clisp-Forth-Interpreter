;; Returns nil if has quote and returns t if not, (this inverse nature is so that (not (every ... )) will work on it
(defun has-quote-p (c)
	(if (char= #\" c) nil t))
(defun str-has-quote-p (str)
  (not (every #'has-quote-p str)))

(defun split-by-one-space (string)
    "Returns a list of substrings of string
    divided by ONE space each.
    Note: Two consecutive spaces will be seen as
    if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))
