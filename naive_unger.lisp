;naive unger attempt 2

(defvar *grammar-ex* 
	'((S  (A B))						;0
		(S  (D C))						;1
		(A ("a"))						;2
		(A ("a" A))						;3
		(B ("b" "c"))					;4
		(B ("b" B "c"))					;5
		(C ("c"))						;6
		(C ("c" C))						;7
		(D ("a" "b"))					;8
		(D ("a" D "b"))))				;9
		
;
; let's create the pseudo-code for the unger parser here.
; How will I do it? 
;	I will start first by using the pseudocode as a description
;	for the routines in question.
;
;	Then create a copy and use this to flesh out the function description
;
; How does it work?
;	The unger parser works by parsing from the top down, using the
;	language grammar as a description
;
;	Start with a sentence and a base rule
;	For each sub-rule derived from the base rule
;		If the sub-rule is a match then									(base case)
;			return the rule
;		Else if the sub-rule is a possible match then					(recursive case)
;			Partition the sentence into m subdivisions 
;				(where m is the number of tokens in the sub-rule)
;			For each partition
;				Call recursive function on each partition
;					(with the current sub-rule as the new base rule)
;				If the call succeeds
;					append function's parse tree to current parse tree
;					continue
;				else return failure
;		If no matches possible											(base case)
;			return failure


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
								(push 
									(reverse (push partial-sentence partitioned-output))
									set-partitions))
							(t 
								(let ((extra-letters (+ 1 (- (length partial-sentence) unfilled))))
									(do ((i 1 (1+ i)))
										((> i extra-letters))
										(gen-part (- unfilled 1)
											(append 
												(list 
													(subseq partial-sentence 0 i))
													partitioned-output)
											(subseq partial-sentence i))))))))
					(gen-part num-partitions () sentence))
				set-partitions))))

(defun unger-parse (sentence)
	(list 'S (unger-aux-derivations sentence *grammar-ex* 'S)))

;Start with a sentence and a base rule	

; Pseudocode header
;	For each production derived from the base rule
;		If the production is a match then								(base case)
;			return the rule
;		Else if the production is a possible match then					(recursive case)
;			Partition the sentence into m subdivisions 
;				(where m is the number of tokens in the sub-rule)
;			For each partition
;				Call recursive function on each partition
;					(with the corresponding token as the new base rule)
;				If the call succeeds
;					append function's parse tree to current parse tree
;					continue
;				else return failure
;		If no matches possible											(base case)
;			return failure
(defun unger-aux-derivations (sentence rule-list nt-symb)
	(dolist (derived-rule 												;For each rule derived from the base symbol
			(match-rule-to-symbol 
				rule-list 
				nt-symb))
			(let ((p-tree (get-production derived-rule)))
				(cond 
					((definite-match sentence derived-rule)				;(base case) If the rule is a match then
						(progn 
							(format t "~a matches ~a~%" sentence derived-rule)
							(return p-tree)))							;return the rule
					((potential-match sentence derived-rule)			;(recursive case) if the rule is a possible match then
						(progn
							(format t "~a is a potential match for ~a~%" sentence derived-rule)
							(let ((result 
										(unger-aux-partitions 			;call partition handler
											sentence 
											rule-list
											derived-rule)))
								(when result							;if it returns a valid result
									(return result)))))					;attach to current parse tree
																		;else continue
					(T 
						(format t "~a does not match ~a~%" sentence derived-rule))))))		;(base case) If no matches possible
															;return failure
															
;this function handles parse-tree generation. Adjust accordingly
(defun unger-aux-partitions (sentence rule-list rule)
	(let* ((prod (get-production rule))
			(num-tokens (length prod))
			(partition-set (gen-partitions num-tokens sentence)))
		(dolist (part-sentence partition-set)
			(print part-sentence)											;For each partitioned sentence
			(let ((p-tree nil))
				(block part-check
					(dotimes (i num-tokens)
						(let ((r-item (nth i prod))							;For each item in the rule
								(p-item (nth i part-sentence)))				;and each corresponding partition
							(format t "r-item is ~a and p-item is ~a~%" r-item p-item)
							(if (symbolp r-item)
								(let ((result 								;if the item is a non-terminal then call parser on it
										(unger-aux-derivations 
											p-item
											rule-list
											r-item)))						;with rule item as the new base rule
									(format t "r-item is a symbol~%")	
									(if result								;if the call succeeds append result to parse tree
										(setf p-tree
											(append p-tree `((,r-item ,result))))
										(progn
											(setf p-tree nil)
											(return-from part-check)))) 	;else break
								(if (equal 
										(list r-item) p-item)				;if item is a terminal symbol then check for a match
									(progn
										(format t "r-item and p-item are equal~%")
										(setf p-tree 
											(append p-tree `((,r-item)))))	;if it matches, append to parse tree
									(progn
										(format t "r-item and p-item are not equal~%")
										(setf p-tree nil)
										(return-from part-check)))))))		;else break
					(when p-tree
						(return p-tree))))))

;	what are the conditions for a definite match?
(defun definite-match (sentence rule)
	(let ((prod (get-production rule)))
		(if (has-nt-syms prod)			;the rule production has no non-terminal symbols
			nil
			(equal sentence prod))))	;the sentence and rule are identical
			
;	what are the conditions for a potential match?
;	(empty rules are not allowed)
(defun potential-match (sentence rule)
	(let ((prod (get-production rule)))
	(if (has-nt-syms prod)				;the rule production has terminal symbols
		(gte-length sentence prod)		;the sentence is longer than the rule
		nil)))

(defun gte-length (lst1 lst2)
	(>= (length lst1) (length lst2)))
	
;checks for the existence of non-terminal symbols in a grammar production
(defun has-nt-syms (prod)
	(if (find-if #'symbolp prod)
		T
		nil))

(defun get-production (rule)
	(second rule))

(defun get-base (rule)
	(first rule))
	
(defun match-rule-to-symbol (rule-list token)
	(remove-if-not 
		#' (lambda (x) (equal (first x) token)) 
		rule-list))
	
(defun match-prod-to-symbol (rule-list token)
	(remove-if-not 
		#' (lambda (x) (search token (first (rest x)))) 
		rule-list))
