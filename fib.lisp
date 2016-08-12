(defun fib (n x y) 
	(if (<= n 1) 
		(if (<= n 0) 0 (+ x y)) 
		(setf x (fib (- n 1) y (+ x y)))))