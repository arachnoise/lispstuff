(defun ranged-list (min max step) 
	(let ((result nil)) 
		(do ((i min (+ i step))) 
			((> i max)) (print i)) 
			(nreverse result)))
			
(defun remove-factors (fac lst) 
	(remove-if 
		#'(lambda (x) (and (> x fac) (= 0 (mod x fac)))) 
		lst))

;2 is the smallest prime number
(defun sieve-erastosthenes (limit) 
	(let 
		((result (ranged-list 1 limit 1)) 
				(max-f (truncate (sqrt limit))))
		(do ((x 2 (1+ x)))
			((> x max-f) result) 
			(setf result (remove-factors x result)))))
