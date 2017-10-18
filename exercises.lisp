;Exercise 1.3.  Define a procedure that takes three numbers as arguments and returns
; the sum of the squares of the two larger numbers. 
; 
; a b c
; 
; idea
; at each step, check value
; is it the biggest of saved values so far?
;	yes - add to head of saved values list
;	no - don't save
;	
;	
;how about a recursive function
;
;start at head
;	if testval > head or null head
;		push testval onto list
;	otherwise
;		call on tail

; what did I find out?
; when creating a variable lisp passes a copy
; it can still point to the original
; getting my head around pointers again
(defun ins-sort-desc (test-val lst)
	(if (or 
			(null lst)
			(> test-val (first lst)))
		(push test-val lst)
		(do ((curr lst (rest curr))
				(next (rest lst) (rest next)))
			((or 
				(null next)
				(> test-val (first next)))
					(setf (cdr curr) 
						(push test-val next))
					lst))))
						
;first solution
(defun ins-sort-desc1 (test-val lst)
	(do ((curr lst (rest curr))
			(build-lst nil (push (first curr) build-lst)))
		((or (> test-val (first curr)) (null curr)) 
			(append
				(nreverse (push test-val build-lst)) 
				curr))))