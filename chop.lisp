(defun chop (splitter str)
  (let ((slice1 (search splitter str)))
    (unless (EQL slice1 nil)
      (let ((slice2 (+ slice1 (length splitter))))
        (list (subseq str 0 slice1) 
          (subseq str slice2 (length str)))))))