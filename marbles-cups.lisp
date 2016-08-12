;sets and bags
;Brent Mercer

;marbles and cups problem basic idea:
;
;There are m marbles in n cups
;m is greater than n
;all cups must have at least 1 marble
;keep it simple: permutations can be generated for each set
;
;idea for evaluation:
;test case by case
;		extra marbles in 1 cup
;		extra marbles in 2 cups
;		...
;   extra marbles in m cups
;
;		extra marbles in 1 cup -> 1 + (m-n) marbles in cup
;		extra marbles in m cups -> 2 marbles in each cup

;
; (1+m-n) 1 1 1 1
; (m-n)	  2 1 1 1
;	(m-n-1) 3 1 1 1
; etc etc etc

;try this
;put all marbles in 1st cup
;put one marble each in 2nd cup
;		put remaining marbles in 1st cup
;		put one marble each in 2 cups
;			put remaining marbles in 1st cup
;		...

;put one marble each in 3 cups
;		put remaining marbles in 1st cup
;		put one marble each in 2nd cup...
;			put remaining marbles in 1st cup
;		...

;suppose there are 6 extra marbles
;one cup: 
;	7 1 1 ...

;two cups: 
;	6 2 1 ...
;	5 3
;	4 4

;three cups
; 5 2 2
; 4 3 2
; 3 3 3

;four cups
; 4 2 2 2 
; 3 3 2 2

;five cups
;3 2 2 2 2 2

;six cups
;2 2 2 2 2 2

;set sizes
;1
;2 1
;2 2 1
;...
;3 1
;3 2 1
;3 2 2 1
;
;4 1
;4 2 1
;
;4 3 1
;
;so set of n cups
;put all marbles in 1 cup
;put all but one in 1st cup
;		start at 2nd cup put all extra marbles in cup
;		start at 2nd cup put all but one in cup
;
;keep taking out marbles and sending to next iteration
;until marbles equals 2nd cup or is 1 greater
;
;this version assumes there are enough marbles to fill all cups
;it doesn't work right!
(defun marbles-cups(num-cups num-marbles)
	;check to make sure there are enough marbles for every cup
	(if (< num-marbles num-cups) (return-from marbles-cups)
		(let ((solution-list '()))
			(labels ((gen-marble-list (m-cup-list cups-left extra-marbles position)
					(cond ((<= cups-left 1)
						(push m-cup-list solution-list))
						((<= extra-marbles 0)
							(push m-cup-list solution-list))
						(t (do ((1st-fill extra-marbles (1- 1st-fill))
										(2nd-fill 0	(1+ 2nd-fill)))
										((< 1st-fill 2nd-fill) nil)
								(let ((rec-mc-list (copy-list m-cup-list)))
									(setf (elt rec-mc-list position) 1st-fill)
									(format t "1st cup ~a 2nd cup ~a position ~a~%" 1st-fill 2nd-fill position)
									(format t "copying list: ~a~%" rec-mc-list)
									(gen-marble-list  rec-mc-list
										(- cups-left 1) 
										2nd-fill 
										(+ position 1))))))))
			(gen-marble-list (make-list num-cups :initial-element 0) num-cups (- num-marbles num-cups) 0))
		solution-list)))

;bags could be defined as a collection of items.
;the pair (x y) represents x occuring y times in the bag
;x is assumed to be unique
;structure is ((item number) (item number) ...)
;I could add a validator routine to enforce that

;Generating permutations - basic algorithm:
;
;pull an item from a bag
;put it in list
;continue until done
;
;I'm not sure if this is faster than the swap method, but I'm happier with it
;It's conceptually cleaner and relates directly to the idea of sets.
;
; put this back in if you ever want it?!
;(format t "uniques: ~a working set: ~a working list: ~a depth: ~a~%" 
;		unique-values working-set working-list perm-length)

(defun gen-perms (the-set perm-length &key (printable nil))
	(let ((set-list '()))
		(labels ((perm (the-set working-list perm-length)
			(if (<= perm-length 0)
				(push working-list set-list)
				(let* ((working-set (copy-list the-set))
						(unique-values (get-items-list working-set)))
					(if (equal unique-values nil)
						(push working-list set-list)
						(dolist (item unique-values)
								(perm (remove-zero-items (draw-from-bag item working-set))
									(append working-list (list item))
									(- perm-length 1))))))))
			(perm the-set '() perm-length))
		set-list))

;this routine draws an item from a bag
(defun draw-from-bag (item a-bag)
	(dolist (cell a-bag)
		(when (equal (first cell) item)
			(let ((number (first (rest cell))))
				(if (<= number 0) nil
					(return-from draw-from-bag 
						(substitute (list item (- number 1)) cell a-bag :test #' equal)))))))

;This function assumes the bag structure is valid
(defun get-items-list (a-bag)
	(let ((item-list '()))
		(dolist (cell a-bag)
			(let ((item (car cell))
					(number (cadr cell)))
				(when	(>= number 1)
					(push item item-list))))
		item-list))

;Workable but didn't work how I expected
;I don't understand bindings and references right, right?
(defun remove-zero-items (a-bag)
		(remove-if #' (lambda (item) 
				(equal (nth 1 item) 0)) a-bag))
		
