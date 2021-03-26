
(defun is_letter(letter)	
	
	(if (alpha-char-p letter) ; CHECK LETTER
		t
		nil)
)


(defun digit_control(digit)		; Digit Control Function
	(if (not(and (> (char-int digit) 47) (< (char-int digit) 58)))
		nil
	t)
)
(defun check_id_with_leading_digits (outputfile temp)
	
	(setq index 0)
	
	(if(equal (char temp 0) #\0)(progn 
		(format outputfile "ERROR ~a~% " temp )
		(return-from check_id_with_leading_digits))
	)
	(loop while(< index (length temp)) do
		(if(and(digit_control (char temp 0))(is_letter (char temp index)))
			(progn
			(format outputfile "ERROR ~a ~%" temp)
			(return-from check_id_with_leading_digits nil))
		)
	(setq index (+ 1 index))
	)
	(return-from check_id_with_leading_digits t)
)
(defun handle_string (temp outputfile)


(cond
				;;controling keywords
				((string-equal temp "")   )
			 	((string-equal temp "exit")   (format outputfile "KW_EXIT ~%"))
			 	((string-equal temp "and")    (format outputfile "KW_AND~%"))
			 	((string-equal temp "or")     (format outputfile "KW_OR~%"))
			 	((string-equal temp "not")    (format outputfile "KW_NOT~%"))
			 	((string-equal temp "equal")  (format outputfile "KW_EQUAL~%"))
			 	((string-equal temp "less")   (format outputfile "KW_LESS~%"))
			 	((string-equal temp "nil")    (format outputfile "KW_NIL~%"))
			 	((string-equal temp "list")   (format outputfile "KW_LIST~%"))
			 	((string-equal temp "append") (format outputfile "KW_APPEND~%"))
			 	((string-equal temp "concat") (format outputfile "KW_CONCAT~%"))
			 	((string-equal temp "set")    (format outputfile "KW_SET~%"))
			 	((string-equal temp "deffun") (format outputfile "KW_DEFFUN~%"))
			 	((string-equal temp "for")    (format outputfile "KW_FOR~%"))
			 	((string-equal temp "if")     (format outputfile "KW_IF~%"))
			 	((string-equal temp "laod")   (format outputfile "KW_LOAD~%"))
			 	((string-equal temp "disp")   (format outputfile "KW_DISP~%"))
			 	((string-equal temp "true")   (format outputfile "KW_TRUE~%"))
			 	((string-equal temp "false")  (format outputfile "KW_FALSE~%"))
			 	((and (eq (char temp 0) #\0) (equal (length temp ) 1))
			 		(format outputfile "VALUE~%")
			 	)
			 	((is_letter (char temp 0)) (format outputfile "IDENTIFIER~%"))
			 	((check_id_with_leading_digits outputfile temp)
			 	(digit_control (char temp 0))(format outputfile "VALUE~%"));;detect the value in a string
			 	
			 	)

)

(defun read_file1 (outputfile inputfile )
	(with-open-file (stream inputfile)
		(setq temp (string ""))
		(setq temp_flag (string ""))
		(setq comment_flag 0)
		(setq comment_counter 0)
		(setq op_flag 0)
		
	    (do ((char (read-char stream nil)
	               (read-char stream nil)))
	        ((null char))

	     	(cond((and (or (is_letter char) (digit_control char)) (equal comment_flag 0))

	     		(setq temp (concatenate 'string temp (string char)))	
	     		(if  (= (length temp_flag) 1)
	     			(progn 
	     				(format outputfile  "OP_MULT ~%" )
	     				(setq temp_flag (string ""))
	     			)

				)
				(if  (= (length temp_flag) 2)
	     			(progn 
	     				(format outputfile  "OP_DBLMULT~%" )
	     				(setq temp_flag (string ""))
	     			)))
	     	)
	     	(cond((and (char/= char #\;) (equal comment_counter 1))
	     			
	      		(setq temp (concatenate 'string temp (string char))) ;; handling my string ;;; checking if its a keyword,id or empty
	     		
	     		(setq comment_counter 0)
	     		(setq comment_flag 0)
	     		(format outputfile  "ERROR  ;~%" )
	     		))


	     	(cond((char= char #\;)
	      		(handle_string temp outputfile) ;; handling my string ;;; checking if its a keyword,id or empty
	     		(setq temp (string ""))
	     		(setq comment_counter (+ comment_counter 1))
	     	
	     		(setq comment_flag 1)))

	     	
	      	
	      	(cond((and (char= char #\,) (equal comment_flag 0)) 
	      			(if  (= (length temp_flag) 1)
	     			(progn 
	     				(format outputfile  "OP_MULT ~%" )
	     				(setq temp_flag (string ""))
	     			))
				(if  (= (length temp_flag) 2)
	     			(progn 
	     				(format outputfile  "OP_DBLMULT~%" )
	     				(setq temp_flag (string ""))
	     			))
	      		(handle_string temp outputfile) ;; handling my string ;;; checking if its a keyword,id or empty
	     		(setq temp (string ""))
	     		;;(format outputfile "OP_OP")
	     		(format outputfile  "OP_COMMA ~%" )	 )

			)
			(cond((and (char= char #\*) (equal comment_flag 0)) 
	      		(handle_string temp outputfile) ;; handling my string ;;; checking if its a keyword,id or empty
	     		(setq temp (string ""))
	     		;;(format outputfile "OP_OP")
	     		(setq temp_flag (concatenate 'string temp_flag (string char)))	

	     		 ))
	      	(cond( (char= char #\newline) 
	      			(if  (= (length temp_flag) 1)
	     			(progn 
	     				(format outputfile  "OP_MULT ~%" )
	     				(setq temp_flag (string ""))
	     			))
					(if  (= (length temp_flag) 2)
	     			(progn 
	     				(format outputfile  "OP_DBLMULT~%" )
	     				(setq temp_flag (string ""))
	     			))
	      		(handle_string temp outputfile) ;; handlng my string ;;; checking if its a keyword,id or empty
	      	
	     		(setq temp (string ""))
	     		(if (and (>= comment_counter 1)(equal comment_flag 1))(cons

	     			(setq comment_flag 0)(cons (setq comment_counter 0)(format outputfile  "COMMENT~%" ))))
	     		))
	      	(cond((and (char= char #\space) (equal comment_flag 0))
	      			(if  (= (length temp_flag) 1)
	     			(progn 
	     				(format outputfile  "OP_MULT ~%" )
	     				(setq temp_flag (string ""))
	     			))
				(if  (= (length temp_flag) 2)
	     			(progn 
	     				(format outputfile  "OP_DBLMULT~%" )
	     				(setq temp_flag (string ""))
	     			))
	      		(handle_string temp outputfile) ;; handling my string ;;; checking if its a keyword,id or empty
	     		(setq temp (string ""))
	     		))
	      	(cond((and (char= char #\+)(equal comment_flag 0))
	      			(if  (= (length temp_flag) 1)
	     			(progn 
	     				(format outputfile  "OP_MULT ~%" )
	     				(setq temp_flag (string ""))
	     			))
				(if  (= (length temp_flag) 2)
	     			(progn 
	     				(format outputfile  "OP_DBLMULT~%" )
	     				(setq temp_flag (string ""))
	     			))
	      		(handle_string temp outputfile) ;; handling my string ;;; checking if its a keyword,id or empty
	     		(setq temp (string ""))
	     		
	     		(format outputfile  "OP_PLUS~%" ) ))
	      	(cond((and (char= char #\-) (equal comment_flag 0))
	      			(if  (= (length temp_flag) 1)
	     			(progn 
	     				(format outputfile  "OP_MULT ~%" )
	     				(setq temp_flag (string ""))
	     			)

				)
				(if  (= (length temp_flag) 2)
	     			(progn 
	     				(format outputfile  "OP_DBLMULT~%" )
	     				(setq temp_flag (string ""))
	     			)

				)
	      		(handle_string temp outputfile) ;; handling my string ;;; checking if its a keyword,id or empty
	     		(setq temp (string ""))
	     		;;(format outputfile "OP_MINUS")
	     		(format outputfile  "OP_MINUS~%" )
	     		 ))

			(cond((and (char= char #\") (equal comment_flag 0))
					(if  (= (length temp_flag) 1)
	     			(progn 
	     				(format outputfile  "OP_MULT ~%" )
	     				(setq temp_flag (string ""))
	     			)

				)
				(if  (= (length temp_flag) 2)
	     			(progn 
	     				(format outputfile  "OP_DBLMULT~%" )
	     				(setq temp_flag (string ""))
	     			)

				)

	      		(handle_string temp outputfile) ;; handling my string ;;; checking if its a keyword,id or empty
	     		(setq temp (string ""))
	     		;;(format outputfile "OP_MINUS")
	     		(if(equal op_flag 0) 
	     			(progn
	     				(format outputfile  "OP_OC~%" )
	     				(setq op_flag 1) 
	     			)
	     			( progn 
	     				(format outputfile  "OP_CC~%" )
	     				(setq op_flag 0) 
	     			))
	     		

	     		 )

	      	)

			(cond ((and ( or (char= char #\?) (char= char #\>) (char= char #\<) (char= char #\_) ) (equal comment_flag 0))
				(handle_string temp outputfile)
				(setq temp (string ""))
			
				(format outputfile  "ERROR  ~a   ~%" char )
			))
	      	(cond((and (char= char #\/) (equal comment_flag 0))
	      			(if  (= (length temp_flag) 1)
	     			(progn 
	     				(format outputfile  "OP_MULT ~%" )
	     				(setq temp_flag (string ""))
	     			)

				)
				(if  (= (length temp_flag) 2)
	     			(progn 
	     				(format outputfile  "OP_DBLMULT~%" )
	     				(setq temp_flag (string ""))
	     			)
				)
	      		(handle_string temp outputfile) ;; handling my string ;;; checking if its a keyword,id or empty
	     		(setq temp (string ""))
	     		;;(format outputfile "OP_DIV")
	     		(format outputfile  "OP_DIV~%" )

	     		 ))
	      	(cond((and (char= char #\() (equal comment_flag 0)) 
	      			(if  (= (length temp_flag) 1)
	     			(progn 
	     				(format outputfile  "OP_MULT ~%" )
	     				(setq temp_flag (string ""))
	     			)

				)
				(if  (= (length temp_flag) 2)
	     			(progn 
	     				(format outputfile  "OP_DBLMULT~%" )
	     				(setq temp_flag (string ""))
	     			)

				)
	      		(handle_string temp outputfile) 
	      		(setq temp (string ""))
	     	
	     		(format outputfile  "OP_OP ~%" )

	     		 )

	      	)
	      	(cond((and (char= char #\)) (equal comment_flag 0))
	      			(if  (= (length temp_flag) 1)
	     			(progn 
	     				(format outputfile  "OP_MULT ~%" )
	     				(setq temp_flag (string ""))
	     			)

				)
				(if  (= (length temp_flag) 2)
	     			(progn 
	     				(format outputfile  "OP_DBLMULT~%" )
	     				(setq temp_flag (string "")))
				)
	      		(handle_string temp outputfile) ;; handling my string ;;; checking if its a keyword,id or empty
	     		(setq temp (string ""))
	     		;(format outputfile "OP_CP")
	     		(format outputfile  "OP_CP  ~%" ))))))

(defun read_from_string  (outputfile str)

	(setq temp (string ""))
	(setq i 0)
	(setq len (length str))
	(setq comment_counter 0)
	(setq comment_flag 0)
	(setq op_flag 0) 
		(loop while (< i len) do 
			(setq char (char str i))
			
			(cond((and (or (is_letter char) (digit_control char)) (equal comment_flag 0))
					
	     		(setq temp (concatenate 'string temp (string char)))	
	     		
				)
	     	)

			(cond((char= char #\;)
			
	      		(handle_string temp outputfile) ;; handling my string ;;; checking if its a keyword,id or empty
	     		(setq temp (string ""))
	     		(if (and (< (+ 1 i) len ) (char= (char str (+ 1 i)) #\;) ) 
	     			(progn 
	     				(setq comment_counter (+ comment_counter 1))
	     				(format outputfile  "COMMENT~%" )
	     				(setq comment_flag 1)
	     				(setq i (+ i 1))	
	     			)
	     			(format outputfile  "ERROR  ~a ~%" char )


	     		 )


	     		))

			(cond( (char= char #\newline) 
	      		(handle_string temp outputfile) ;; handlng my string ;;; checking if its a keyword,id or empty
	      		;;(format outputfile temp)
	     		(setq temp (string ""))
	     		(if (and (>= comment_counter 1)(equal comment_flag 1))(cons

	     			(setq comment_flag 0)(cons (setq comment_counter 0)(format outputfile  "COMMENT~%" ))))
	     		)
			)
			

			(cond((and (char= char #\-) (equal comment_flag 0))
	      			(handle_string temp outputfile) ;; handling my string ;;; checking if its a keyword,id or empty
	     			(setq temp (string ""))
	     		;;(format outputfile "OP_MINUS")
	     			(format outputfile  "OP_MINUS~%" )
				
			))
			(cond((and (char= char #\+) (equal comment_flag 0))
	      			(handle_string temp outputfile) ;; handling my string ;;; checking if its a keyword,id or empty
	     			(setq temp (string ""))
	     		;;(format outputfile "OP_MINUS")
	     			(format outputfile  "OP_PLUS~%" )
				
			))
			(cond((and (char= char #\/) (equal comment_flag 0))
	      			(handle_string temp outputfile) ;; handling my string ;;; checking if its a keyword,id or empty
	     			(setq temp (string ""))
	     		;;(format outputfile "OP_MINUS")
	     			(format outputfile  "OP_DIV~%" )
				
			))

			(cond((and (char= char #\)) (equal comment_flag 0))
	      			(handle_string temp outputfile) ;; handling my string ;;; checking if its a keyword,id or empty
	     			(setq temp (string ""))
	     		;;(format outputfile "OP_MINUS")
	     			(format outputfile  "OP_CP  ~%" )
				
			))

			(cond((and (char= char #\() (equal comment_flag 0))
	      			(handle_string temp outputfile) ;; handling my string ;;; checking if its a keyword,id or empty
	     			(setq temp (string ""))
	     		;;(format outputfile "OP_MINUS")
	     			(format outputfile  "OP_OP  ~%" )
				
			))
			(cond((and (char= char #\Space) (equal comment_flag 0))
	      			(handle_string temp outputfile) ;; handling my string ;;; checking if its a keyword,id or empty
	     			(setq temp (string ""))
	     		;;(format outputfile "OP_MINUS")
	     			
				
			))
			(cond ((and ( or (char= char #\?) (char= char #\>) (char= char #\<) (char= char #\_) ) (equal comment_flag 0))
				(handle_string temp outputfile)
				(setq temp (string ""))
			
				(format outputfile  "ERROR  ~a   ~%" char )
			))

			(cond((and (char= char #\") (equal comment_flag 0))
	      			(handle_string temp outputfile) ;; handling my string ;;; checking if its a keyword,id or empty
	     			(setq temp (string ""))

	     		(if(equal op_flag 0) 
	     			(progn
	     				(format outputfile  "OP_OC~%" )
	     				(setq op_flag 1) 
	     			)
	     			( progn 
	     				(format outputfile  "OP_CC~%" )
	     				(setq op_flag 0) 
	     			))
	     			
				
			))
			
			(cond((and (char= char #\*) (equal comment_flag 0))
				(handle_string temp outputfile) ;; handling my string ;;; checking if its a keyword,id or empty
	     			(setq temp (string ""))
				(if (and (< (+ 1 i) len ) (char= (char str (+ 1 i)) #\*)) 
					(progn 
	     				
	     				(format outputfile  "OP_DBLMULT~%" )
	     				
	     				(setq i (+ i 1))	
	     			)
					(format outputfile  "OP_MULT~%" )
					)
	      			
	     		;;(format outputfile "OP_MINUS")
	     			
				
			))




			(setq i (+ i 1))
		)
)

(defun write_to_file (outputfile inputfile str )

	(with-open-file (stream outputfile
										:direction :output 
										:if-exists :supersede
	   									:if-does-not-exist :create)
		(if (string-equal inputfile NIL) 
			(loop while(not (string-equal str "")) do
				(read_from_string stream str)
				(setf str (read-line))

			)

			(read_file1 stream inputfile )

		 )

	 	
))
(defun gppinterpreter(&optional filename )

	(cond( (string-equal filename NIL)
		(setf st (read-line))
		( write_to_file "parsed_lisp.txt" filename st)
		
		)
		
	)
	(cond ((not (string-equal filename NIL))
			(write_to_file "parsed_lisp.txt" filename ""))

	)
)
(defun main ()

    (if (null *args*)
        (gppinterpreter)
        (gppinterpreter (car *args*))
    )

    
)
(main)
