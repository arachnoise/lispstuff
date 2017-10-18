;str operations

;tokenizing
;mark non-terminals position and length
;then split into pieces

(load (merge-pathnames "list_ops.lisp" *load-pathname*))
(load (merge-pathnames "treewalk.lisp" *load-pathname*))

;(load "D:/documents/programming/lisp/list_ops.lisp")
;(load "D:/documents/programming/lisp/treewalk.lisp")

(defun tokenise (strs delimiters &key keep-d)
	(let ((token-list (make-as-list strs)))
		(do ((test-delim (make-as-list delimiters) (rest test-delim))
					(int-token-list nil nil))
			((null test-delim))
			(dolist (str token-list)
				(push 
					(get-toks (first test-delim) str :keep-d keep-d) 
					int-token-list))			
			(setf token-list 
				(nreverse 
					(collect-leaves int-token-list))))
		token-list))

(defun get-toks (delimiter str &key keep-d)
	(let ((t-list nil))
		(if (zerop (length delimiter))
			(return-from get-toks (expand-str str))
			(do ((tok 
						(multiple-value-list (next-token delimiter str :reset t))
						(multiple-value-list (next-token delimiter str))))
				((null (first tok)))
				(push (first tok) t-list)
				(when (and keep-d (second tok))
					(push delimiter t-list))))
		(remove "" t-list :test #'equal)))

(let ((start-tok 0 ) (end-tok nil))
	(defun next-token (delimiter str &key reset)
		(when reset
			(setf end-tok (- 0 (length delimiter))))
		(setf start-tok 
			(when end-tok
				(+ (length delimiter) end-tok)))
		(when start-tok
			(values
				(subseq str start-tok
					(setf end-tok 
						(search delimiter str :start2 start-tok)))
				end-tok))))

;this function expects a list for match-list

(defun mark-positions (match-list search-seq)
	(let ((found-list nil))
		(labels ((gen-f-list (m-list prev-pos)
				(cond ((null m-list)
						(return-from gen-f-list))
						(t
							(let* ((item (first m-list)) 
									(found-pos 
										(search item search-seq :start2 prev-pos)))
								(cond (found-pos
										(push (cons found-pos item) found-list)
										(gen-f-list m-list (+ found-pos (length item))))
									(t
										(gen-f-list (rest m-list) 0))))))))
			(gen-f-list match-list 0))
		(sort found-list 
			#'(lambda (x y) (< (first x) (first y))))))
		
(defun remove-all-char (char str)
	(remove char str :count (count char str)))
	
(defun remove-substr (substr str)
	(let ((found-pos (search substr str)))
		(if found-pos
			(let ((end-pos 
						(+ found-pos (length substr))))
				(concatenate 'string
					(subseq str 0 found-pos)
					(subseq str end-pos))) 
			str)))
			
(defun remove-all-substr (substr str)
	(let ((found (search substr str)))
		(if found
			(remove-all-substr substr 
				(remove-substr substr str))
			str)))
			
(defun prune-string (prune-list str 
		&optional (prune-fn #' remove-all-substr))
	(if prune-list 
		(prune-string 
			(rest prune-list) 
			(funcall prune-fn (first prune-list) str) 
			prune-fn)
		str))	

;expand-specials function group
;
;expand str turns a string into a letter by letter expansion
;if mark-specials is enabled, expand-str treats 
;special sequences like individual letters

(let ((expand-specials nil))
	(defun expand-string 
		(str &key mark-specials
			(special-chars expand-specials))
		(if mark-specials
			(let ((found-specials 
						(mark-positions special-chars str))
					(str-expansion nil)
					(str-length (length str)))
				(labels ((str-exp-spec (curr-mark index)
						(cond 
							((>= index str-length)
								(return-from str-exp-spec))
							((and curr-mark 
									(= index (caar curr-mark)))
								(push (cdar curr-mark) str-expansion)
								(str-exp-spec 
									(rest curr-mark) 
									(+ index (length (cdar curr-mark)))))
							(t
								(push 
									(string (elt str index)) 
									str-expansion)
								(str-exp-spec curr-mark (1+ index))))))
					(str-exp-spec found-specials 0))
				(nreverse str-expansion))
		(loop for char across str
				collect (string char))))
				
	(defun new-expand-specials (new-specials)
		(setf expand-specials 
				(remove-if 
					#'(lambda (x) (equal x ""))		;zero-strings lead to infinite recursion!
					new-specials)))
					
	(defun get-expand-specials ()
		expand-specials))
		
(defun chop (splitter str  &key (destroy-splitter t))
  (let ((slice1 (search splitter str)))
    (if (EQL slice1 nil)
			str
      (let ((slice2 (+ slice1 (length splitter))))
				(if destroy-splitter
					(list (subseq str 0 slice1) 
						(subseq str slice2 (length str)))
					(list (subseq str 0 slice1)
						splitter
						(subseq str slice2 (length str))))))))
