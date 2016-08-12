;n unique items in m partitions
;
;		
;1 1 4
;1 4 1
;4 1 1


;for each unique number
;put at head of set
;


;;;generating permutations from a set of n unique items
;;;viewed as a case of recursion
;;;
;;;base cases:
;;;empty set -> return empty set
;;;set of one -> return set
;;;
;1 2 3 4
;for shifting (n-1 n) 3 possible
;for shifting (n-2 n) 2 possible
;for shifting (n-3 n) 1 possible
;shift equivalence
;shift (2 4) equals shift (3 4) shift(2 3) shift (3 4)
;shift (n-2 n) equals 
;	shift (n-1 n) 
;	shift (n-2 n-1)
;	shift (n-1 n)

;1 2 3 4
;1 2 4 3
;1 4 2 3
;1 4 3 2

;shift (n-3 n) equals
;	shift (n-1 n)
;	shift (n-2 n-1)
;	shift (n-3 n-2)
;	shift (n-2 n-1)
;	shift (n-1 n)
;1 2 3 4
;1 2 4 3
;1 4 2 3
;4 1 2 3
;4 2 1 3
;4 2 3 1

;so defining a swap (n-m n) in terms of shifts entails 2m-1 shifts
;	shift (n-1 n)
;	...
;	shift (n-m n-m+1)
;	...
;	shift (n-1 n)

;for set size 3 ex
;
;2 levels
;no swap swap (1 2) (swap 1 3)
;set 1 
;	no swap swap (2 3)
;set 2 
;	no swap swap (2 3)
;set 3 
;	no swap swap (2 3)

;1 2 3
;	1 3 2
;2 1 3
;	2 3 1
;3 2 1
;	3 1 2

;for set size 4 ex
;3 levels
;no swap swap (1 2) swap (1 3) swap (1 4)
;
;rec case 
;	no swap swap (2 3) swap (2 4)
;base
;	no swap swap (3 4)
;
;
;for a depth of n levels this will put 
;(n + (n-1) + (n-2) + ... + 1) levels on the stack
;
;(n+1)*(n/2) or (n*(n+1))/2
;summation (1 10) -> 55
;summation (1 11) -> 66
;
;or will it? Maybe I've misunderstood how stacks work. Let's check:
;maybe at most n levels deep (recursion is tres powerful, no?)
;1 2 3 4
;	1 2 3 4
;		1 2 3 4
;		1 2 4 3
;	1 3 2 4
;		1 3 2 4
;		1 3 4 2
;	1 4 3 2
;		1 4 3 2
;		1 4 2 3
;2 1 3 4
;	2 1 3 4
;		2 1 3 4
;		2 1 4 3
;	2 3 1 4
;		2 3 1 4
;		2 3 4 1
;	2 4 3 1
;		2 4 3 1
;		2 4 1 3
;3 2 1 4
;	3 2 1 4
;		3 2 1 4
;		3 2 4 1
;	3 1 2 4
;		3 1 2 4
;		3 1 4 2
;	3 4 1 2
;		3 4 1 2
;		3 4 2 1
;4 2 3 1
;	4 2 3 1
;		4 2 3 1
;		4 2 1 3
;	4 3 2 1
;		4 3 2 1
;		4 3 1 2
;	4 1 3 2
;		4 1 3 2
;		4 1 2 3

;establishing a recursive case:
;	for a set of size n-m
;		call for unmodified set
;		call for swap(m x) of set where x ranges from m+1 to n
;	

;how about:
; for a given set of size n
;
; base case:
; if subset size is 1 return set as list
;
; work on a subset size n-m starting at position m in set
; for a value x ranging from m to n
; 	work on subset size n-(m+1) starting at m+1 in set
;		with swap (x m)

;generates unique permutations for a set
;
;this method fails for sets with multiple groups of duplicate items
;Why? 
;Because the recursive method has no way of testing
;outside of its local context.
;
;I will put a band-aid on this, but it's a dirty no-good hack.
;There has to be a better method
;has been done before without testing the entire set

(defun gen-perms (the-set)
	(let ((set-list ()))
		(labels ((perm (the-set m n)
			(let ((size (- n m)))
				(if (<= size 0)
					(format t "~%        ~a depth ~a final value~%~%" the-set m)
					;(unless (find the-set set-list :test #' equal)
					;		(push the-set set-list))
					(progn 
						(format t "~a depth ~a ~%" the-set m)
						(perm the-set (1+ m) n)
						(do ((x (1+ m) (1+ x))) ;replace with (1+ m) when done
							((>= x (1+ n)))
							(unless (eql (nth x the-set) (nth m the-set))
								(format t "~a depth ~a swapping ~a~%" the-set m x)
								(perm (swap the-set x m) (1+ m) n))))))))
			(perm the-set 0 (1- (length the-set)))
			set-list)))
		

(defun swap (the-set x y)
	(let ((prv-set (copy-seq the-set)) (z (nth x the-set))) 
		(setf (nth x prv-set) (nth y prv-set)) 
		(setf (nth y prv-set) z) 
		prv-set))

