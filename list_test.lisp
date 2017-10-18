
(defvar *grammar-ex* 
	'((S  (A B))						;0
		(S  (D C))						;1
		(A ("a"))							;2
		(A ("a" A))						;3
		(B ("b" "c"))					;4
		(B ("b" B "c"))				;5
		(C ("c"))							;6
		(C ("c" C))						;7
		(D ("a" "b"))					;8
		(D ("a" D "b"))))		;9

;for input-list on output tree
			
;look up rule
;for each symbol in rule
;	take next list item and insert as child of output tree
; recurse
(defun make-p-tree (input)
	(when (listp input)
		(let ((work-list input)
					(p-tree-base '(S)))
			(labels 
				((rec-p-tree (p-tree)
						(format t "at rec call begin working list is ~a~%" work-list)
						(format t "at rec call begin p-tree is ~a~%" p-tree)
						(if work-list
							(progn 
								(let ((prod 
											(cadr
												(nth (first work-list) *grammar-ex*))))
									(format t "list call production is ~a~%" prod)
									(if p-tree
										(setf p-tree
												(append p-tree	(list (list (pop work-list)))))
										(setf p-tree (list (pop work-list))))
												
									(format t "after first assignment p-tree is ~a~%" p-tree)
													
									(do ((sym (pop prod) (pop prod))
												(curr p-tree (rest curr)))
										((or (null sym) (null curr)) p-tree)
											
										(format t "list call symbol is ~a~%" sym)
										(format t "current item starts as ~a~%" curr)
										(when (symbolp sym)										
											(setf (rest curr)
													(list (rec-p-tree (second curr))))
											(format t "current item is now ~a~%" curr)
											(format t "after final assignment p-tree is ~a~%" p-tree))))))
								(format t "end of rec call p-tree is ~a~%" p-tree)
								p-tree))
				(rec-p-tree p-tree-base))
				p-tree-base)))

;I did it! It's short and beautiful!
;I wonder if it could be shorter
;
;Creates a structured parse tree from a left-corner derivation list
;
;Doesn't validate the list to see if it's a valid parse tree (how would I do that?)
(defun make-p-tree2 (input)
	(when (listp input)
		(let ((work-list input))
			(labels 
				((rec-p-tree ()
						(when work-list
							(let 
								((prod 
										(prod-from-num (first work-list)))
									(p-tree (list (pop work-list))))
								(dolist (sym prod)	
									(when (symbolp sym)										
										(setf p-tree
											(append p-tree (rec-p-tree)))))
							(list p-tree)))))		;encapsulate to protect contents from append
				(first (rec-p-tree))))))

;assumes grammar rule structure corresponds to that of *grammar-ex*
(defun possible-match (rule sentence)
	(let* ((prod (second rule))
				(len-p (length prod))
				(len-s (length sentence)))
		(if	(has-symbols prod)
			(<= len-p len-s)
			(equal prod sentence))))

(defun match-parts (rule possible)
	(let ((accum nil))
		(dolist (divided possible)
			(dolist (partition divided)
				(push 
					(maplist 
						#'(lambda (x) (print x)
						(possible-match rule x))
						partition)
					accum)))
		(nreverse accum)))
			
	
(defun has-symbols (lst)
	(dolist (elt lst)
		(when (symbolp elt)
			(return-from has-symbols T))))
			
(defun no-syms (lst)
	(remove-if-not #'symbolp lst))

(defun productions (base-rule)
	(let ((accum nil))
		(mapc 
			#'(lambda (x)
				(if (eql (first x) base-rule)
					(push (second x) accum)))
			*grammar-ex*)
		(nreverse accum)))

(defun prod-from-num (rule-num)
	(cadr (nth rule-num *grammar-ex*)))


;generates possible partitions for an input sentence
;m partitions
;n length for sentence
;position
(defun gen-partitions (num-partitions sentence)
	(cond ((> num-partitions (length sentence))
			(return-from gen-partitions))
		((< num-partitions 1)
			(return-from gen-partitions))
		(t 
			(let ((set-partitions ()))
				(labels
					((gen-part (unfilled partitioned-output partial-sentence)
						(cond ((<= unfilled 1)
								(push (reverse (push partial-sentence partitioned-output))
									set-partitions))
							(t 
								(let ((extra-letters (+ 1 (- (length partial-sentence) unfilled))))
									(do ((i 1 (1+ i)))
										((> i extra-letters))
										(gen-part (- unfilled 1)
											(append (list (subseq partial-sentence 0 i)) partitioned-output)
											(subseq partial-sentence i))))))))
					(gen-part num-partitions () sentence))
				set-partitions))))
				
(defun t-insert (tree elt)
	(if elt
		(cond 
			((rest tree)
			(append `( ,(first tree) (,elt)) 
				(rest tree)))
			(tree
				`( ,(first tree) (,elt)))
			(t
				(list elt)))
		tree))
