;insert into list after position
(defun insert (item lst)
	(cond ((null lst) 
			(make-as-list item))
		((listp lst)
			(cons (first lst)
					(append (make-as-list item) (rest lst))))
		(t
			(cons lst (make-as-list item)))))

;side-effect: changes cons cell for list pointer
(defun ninsert (item lst)
	(cond ((null lst) 
			(make-as-list item))
		((listp lst)
			(setf (cdr lst)
					(append (make-as-list item) (rest lst))))
		(t
			(cons lst (make-as-list item)))))


(defun make-as-list (try-item)
	(unless (listp try-item)
		(list try-item)))