;A way to learn lisp

;Implement basic versions of parsing techniques.
;I would do this to see if my approach is sound, and if I properly
;understand the algorithms.

;today I will try implementing a naive Unger style parser for CF grammars.
;It has an exponential time but refining the algorithm can reduce this to
;polynomial time.

;How does Unger's work?
;It works like putting marbles in cup
;If I have a sentence "pqrs" I could partition the terminals like so
;
;For example:
;	The grammar S->EFG S->HI S->J
;
;-------------------------------
;				S
;-----------+-----------+-------
;	E		| 	F		| 	G
;-----------+-----------+-------
;	pq		|	r		|	s
;	p		|	qr		|	s	
;	p		|	q		|	rs
;
; So given a rule, the parser will search from available possibilities
;
;
;	Unger's parser works from the top down, applying rule-list and creating a
;parse tree for a sentence (if the rule-list match)
;
;Now, given a definition of rule-list, can I parse a string?

;Try a sample grammar

;S -> aSb | ab

;What needs to be done
;read input file
;create grammar rule-list from file input
;store grammar rule-list as a list of rule-production pairs
;
;Applying rule-list
;This could be done via a simple string substitution
;
;Creating a parse-tree
;Can use a left-most tree derivation to store tree as a list
;
;Implementing a control mechanism
;No idea yet
;
;Some questions on implementation:

;How do I represent rule-list and productions?
;How do I store parse trees and predictions?
;How do I transform strings based on rule-list?

;rule-list are represented as 3 sections
;LHS assignment RHS

;validating rule-list - cases:
;correct
;empty line
;no assignment operator
;lhs empty
;lhs more than one symbol
;lhs doesn't define rule with substitution specifier
;lhs uses substitution specifier too many times
;rhs empty

(load "D:/documents/programming/lisp/treewalk.lisp")
(load "D:/documents/programming/lisp/grammar_ops.lisp")
(load "D:/documents/programming/lisp/string_ops.lisp")

;move grammar analysis to own file
;move input reader to own file
;move string operations to own file

(defvar *grammar-ex* 
	'(("$S" "$A$B")
		("$S" "$D$C")
		("$A" "a")
		("$A" "a$A")
		("$B" "bc")
		("$B" "b$Bc")
		("$C" "c")
		("$C" "c$C")
		("$D" "ab")
		("$D" "a$Db")))


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
		
;an unger parser works from the top down,
;searching through a forest of possible parse trees
(defun unger-parse (rule-list sentence)
	(let ((parse-trees nil))
		(new-expand-specials 
				(grammar-non-terms rule-list))
		(rec-unger-parse rule-list sentence "$S" parse-trees)
		parse-trees))
;start with start-symbol $S
;look for matching rule-list to this
;create partitions for partial sentence
;try each rule recursively


;this version is big and ugly
;
;
(defun rec-unger-parse (rule-list sentence-part parse-item parse-tree)
	(let ((found-match nil))
		(if (< 0 (length 
					(remove-terminals rule-list parse-item)))
			(let ((matched-rules
							(match-rule-to-symbol rule-list parse-item)))
							
				(format t "~%matching rules for ~a are ~a" parse-item matched-rules)
				
				(dolist (match matched-rules)
					(let* ((prod-list 
								(expand-string 
									(second match) :mark-specials t))
								(partition-list 
									(gen-partitions
										(length prod-list)
										sentence-part)))		
										
						(format t "~%attempting match rule ~a to sentence ~a" match sentence-part)
						(print prod-list)
						(print partition-list)
						
						(dolist (curr-part partition-list)
						
							(format t "~%For partition set ~a" curr-part)
							
							(do 
								((part curr-part (rest curr-part))
									(parse-items prod-list (rest parse-items)))
								((or 
									(null part) 
									(null parse-items)))
									
								(format t "~%matching ~a to ~a length ~a"
									(first parse-items)
									(first part)
									(length (first part)))	
									
								(match-msg 
									(setf found-match
										(rec-unger-parse 
											rule-list 
											(first part) 
											(first parse-items)
											parse-tree))
									(first parse-items) 
									(first part))
								(unless found-match (return)))))))
			(setf found-match (equal sentence-part parse-item)))
		found-match))
		
(defun match-msg (ifif item-a item-b)
	(let ((msg-yes "~%~a matches ~a")
				(msg-no "~%~a doesn't match ~a"))
	(if ifif
		(format t msg-yes item-a item-b)
		(format t msg-no item-a item-b))))