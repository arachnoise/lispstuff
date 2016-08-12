;;;;reading a file
;;;;Open a file and read a line at a time

;;;I'm going to try using this to test parsing
;;;But first I'm going to try printing a file

;;;so read line: print line

(with-open-file 
	(stream "D:/documents/programming/lisp/grammar1.txt")
	(loop for line = (read-line stream nil) 
		while line do (format t "~a~%" line)))
		
;;;to convert one line to another
;;;For example:
;;;	$S := $Expr
;;; in LHS side $ begins name and " " ends it
;;; $ is a marker for substitution
;;; chop it up
;;; Becomes "$S" check for := and then "$Expr"

;;;chops a string into two halves
;;;returns a list containing them
;;
;;conditions to match:
;;not in list
;;beginning
;;end
