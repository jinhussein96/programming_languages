(defun read_from_file (filename) ;; reads the input file into a list 
(with-open-file (in filename)
	(loop for liste = (read-line in nil nil)
		while liste 
		
		collect (read-from-string  (concatenate 'string "(" liste ")" ))))

	)




(defun if_even (num) 
		(if (= (mod num 2) 0) t)
)
(defun if_odd (num) 
		(if (/= (mod num 2) 0) t)
)

 (defun collatz_sequence (num stream)
 	(cond
	 	((= num 1)
	 		(format stream "1 ~%")
	 		(return-from collatz_sequence t)
	 	)
		((if_even num) 
			(cons
					(format stream "~S " num ) 
					(collatz_sequence (/ num 2) stream)
		    )
		);
		((if_odd num) 
			(cons
					(format stream "~S " num ) 
					(collatz_sequence (+ (* num 3) 1) stream)
			);

		) 	
	)
 )

 (defun write_to_file (num)

(with-open-file (stream "collatz_outputs.txt"
									:direction :output 
									:if-exists :supersede
									:if-does-not-exist :create)
(setq count 0)
 	
 	(loop for x from 0 to 4
  		do(cond 
 				((car num)
	 				(format stream "~S: " (car num) )
	 				(collatz_sequence (car num) stream )
	 				(setq num (cdr num))
	 				
	 					
	 				
	 			)


 			)
  	)
 	
 )

)
 (defun flattener (nested_list) 
	
	(cond 
		;; returns nil when the list is empty
		((null nested_list) nil )	
		
		;; if the first element of the given list is also a list 
		;; it calls flattener function with that list 

		((listp (car nested_list))

			 (append (flattener (car nested_list)) (flattener (cdr nested_list)))
		)
		(t ;; if the other actions are faulse then this condition will be performed 
			;;which is appending the first element to the list and calling the flattener func again 
			(append (list (car nested_list)) (flattener (cdr nested_list)))
		)
		
	)
		
)
	
 
 (defun main () 

 	(setq my_list (flattener (read_from_file "integer_inputs.txt")) )
 	(write_to_file my_list)
 	

 )

(main)