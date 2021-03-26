
(defun read_from_file (filename) ;; reads the input file into a list 
(with-open-file (in filename)
	(loop for line = (read-line in nil nil)
		while line collect line))
)



(defun freq (str)
	(let ((ht (make-hash-table))) ;;creating  A new hash table
(loop while str do 						;;taversing the sequence of strings .. each new line is a string 
		(loop for c across (car str)    ;; going throw a string 
			do
	        (incf (gethash c ht 0))     ;; increament the letter counts 
	    )
	    (setq str (cdr str))
 )
	    
		 (return-from freq ht)

	)

)
; convert the hash table into an association list and then sorting it according to the frequences (the key values )
(defun sort_table (ht)
  (let ((my_list nil))
    (maphash (lambda (k v)
               (push (cons k v) my_list)
             )
             ht
    )
     (subseq (sort my_list #'< :key #'cdr) 0 (hash-table-size ht ))
   )
)




(setq ht (freq (read_from_file "paragraph.txt")))
(setq my_list (sort_table ht))
(print my_list)



