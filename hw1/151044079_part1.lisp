
(defun read_from_file (filename) ;; reads the input file into a list 
(with-open-file (in filename)
	(loop for line = (read-line in nil nil)
		while line collect (read-from-string  (concatenate 'string "(" line ")" ))))
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
	

;;writing the output to the file 
;;
(defun write_to_file (intput_filename output_filename)

	(with-open-file (output_file output_filename :direction :output 
									:if-exists :supersede
									:if-does-not-exist :create)
 (format output_file "~{ ~A~} " (flattener  (read_from_file intput_filename))))

	)



;;my main function 

(defun main ()
(write_to_file "nested_list.txt"  "flattened_list.txt" ))

;;(print (format nil "~{ ~A~} " (flattener  (read_from_file "1.txt"))))
 
(print (flattener (read_from_file "nested_list.txt" )))

;;calling my main function 
(main)