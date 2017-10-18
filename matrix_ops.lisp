;add two arrays
;
;Method 1: use map function
;Method 2: add manually using row-major order
;
;Enforce dimensionality
(defun add-matrices (x y)
  (reduce-matrix-list #'+ x y))

(defun sub-matrices (x y)
  (reduce-matrix-list #'- x y))

;performs an element to element matrix operation for a list of matrices
;
;operation starts with the first array
;Overall: operation is done left to right
;For each array in row-major order
;
;Any ops are allowed, but I'm really writing it to use matrix-add and matrix-sub
;as wrappers.
;
;It might prove useful in other cases as well.
(defun reduce-matrix-list (op-fn &rest args)
  (if (eq-dimensions args)
	(let* ((x (first args))
		  (dim-x (array-dimensions x))
		  (result (make-array dim-x :initial-element 0))
		  (num-rows (first dim-x))
		  (num-cols (second dim-x)))
	  (dotimes (i num-rows)
		(dotimes (j num-cols)
		  (setf (aref result i j)
				(aref x i j))))
	  (dolist (y (rest args))
		  (dotimes (i num-rows)
			(dotimes (j num-cols)
			  (setf (aref result i j)
					(funcall op-fn (aref result i j)
					   (aref y i j))))))
	  result)
	nil))

(defun apply-to-matrix (mat expr)
  (let* ((dim-mat (array-dimensions mat))
		  (num-rows (first dim-mat))
		  (num-cols (second dim-mat)))
	(dotimes (i num-rows)
	  (dotimes (j num-cols)
		(setf (aref mat i j))))))

;returns true if all arrays have the same number of dimensions
(defun eq-dimensions (args)
  (let* ((eq-val nil)
		(x (first args))
		(dim-x (array-dimensions x)))
		(dolist (y (rest args))
		  (let ((dim-y (array-dimensions y)))
			(setf eq-val (equal dim-x dim-y))))
		eq-val))

(defun transpose ())
;multiply two vectors or matrices (cross product)
;transpose a vector
;check number of rows and columns
;
;sanity-check: enforce matrix constraints. Any ill-defined matrix will not be accepted
;define a vector as row(x y z) and column ((x) (y) (z))
;
;3x2 matrix is ((u v) (w x) (y z))

