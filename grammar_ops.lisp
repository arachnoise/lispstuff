
;this is hard-coded to a specific notation definition
;how do I abstract this?

(defun create-rule (string)
	(let* 
		((substitution-specifier #\$) 
			(divider ":=") 
			(a-rule	(chop divider string)))
		(when (listp a-rule)
			(dolist (str a-rule) 
				(nsubstitute  
					(string-trim '(#\Space #\Tab #\Newline) str)
					str
					a-rule
					:test #' equal))
			(if 
				(and 
					(not (find "" a-rule :test #' equal))
					(not (find #\space (first a-rule)))
					(>= (length (first a-rule)) 2)
					(equal (position substitution-specifier (first a-rule)) 0)
					(equal (count substitution-specifier (first a-rule)) 1))
				a-rule nil))))

;only works with valid (A -> B) rule-list
(defun apply-rule (rule str)
	(let ((match (chop (first rule) str)))
		(when (listp match)
			(concatenate 'string 
				(first match) 
				(first (rest rule))
				(first (rest match))))))
			
(defun apply-rule-from-list (rule-list rule-num str)
	(apply-rule (nth rule-num rule-list) str))

;FIX-ME
;this definition is limited, as it will not find non-reachable
;non-terminals for a given grammar.

(defun grammar-non-terms (rule-list)
	(remove-duplicates 
		(mapcar #' first rule-list) :test #' equal))

(defun grammar-terminals (rule-list)
	(remove-duplicates 
		(collect-leaves 
			(mapcar #' (lambda (x) 
					(expand-string (remove-non-terms rule-list x))) 
				(get-productions rule-list)))
		:test #'equal))

(defun get-rules (rule-list)
	(mapcar #' first rule-list))
	
(defun get-productions (rule-list)
	(mapcar #' second rule-list))
		
(defun remove-non-terms (rule-list sentence)
	(prune-string (grammar-non-terms rule-list) sentence))

(defun remove-terminals (rule-list sentence)
	(let ((found-terms 
					(expand-string 
						(remove-non-terms rule-list sentence))))
		(prune-string found-terms sentence)))

(defun get-terminal-list (rule-list sentence)
	(expand-string
		(remove-non-terms rule-list sentence)))

(defun get-non-term-list (rule-list sentence)
	(let ((non-terms (grammar-non-terms rule-list)))
		(expand-string
			(remove-terminals rule-list sentence)
			:mark-specials t
			:special-chars non-terms)))

(defun match-rule-to-symbol (rule-list token)
	(remove-if-not 
		#' (lambda (x) (equal (first x) token)) 
		rule-list))
	
(defun match-prod-to-symbol (rule-list token)
	(remove-if-not 
		#' (lambda (x) (search token (first (rest x)))) 
		rule-list))
