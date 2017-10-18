(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) 
      (princ a s))))

(defun symb (&rest args) 
  (values 
    (intern 
      (apply #'mkstr args))))

(defun begins-with-minus (x) 
  (and 
    (symbolp x) 
    (> (length (symbol-name x)) 1) 
    (string= "-" (symbol-name x) :start2 0 :end2 1)))

(defun neg-to-pos-symbol (s) 
  (symb 
    (subseq (symbol-name s) 1)))

(defun expand-unary-minus (x) 
  (if (begins-with-minus x) 
    `(- 0 ,(neg-to-pos-symbol x)) 
    x))

(defmacro with-unary-minus (expr))
