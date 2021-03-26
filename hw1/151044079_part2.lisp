(defun read_from_file (filename) ;; reads the input file into a list 
(with-open-file (in filename)
	(loop for liste = (read-line in nil nil)
		while liste 
		
		collect (read-from-string  (concatenate 'string "(" liste ")" ))))

	)

(defun isPrime (num1 num2)  ;checks if num1 is a prime number or not ,,,, 

	(cond
		((= num2 1) t) 		;          ; return true if num2 equals to one
		((<= num1 1) nil)             ; return nil when num1 is less than one
		((< num2 1) nil)              ;; returns nil when num2 is less than one
		((= (mod num1 num2) 0) nil)  ;; condition for non prime 
		(t
		(isPrime num1 (- num2 1))
		)

	)



)
(defun is_semiPrime (num1 num2 counter counter2 ) ;; checks if the given num1 is a semi prime or not 
												  ;; counter for the prime divisors 
												  ;;counter2 for the non prime divisors 

	(cond
		((<= num1 0) nil)                         
		((<= num2 0) nil)
		;;((isPrime num1 (- num1 1)) nil)           
		((and (= counter 2) (= counter2 0) (= num2 1) ) t)  
		((and (= (mod num1 num2 ) 0) (isPrime num2 (- num2 1)) 
			(if (= (* num2 num2) num1)
				(is_semiPrime num1 (- num2 1) (+ counter 2) counter2)
				(is_semiPrime num1 (- num2 1) (+ counter 1) counter2))))
		((= (mod num1 num2 ) 0)  
		(is_semiPrime num1 (- num2 1) counter  (+ counter2 1 ))
		)
		(t
			(is_semiPrime num1 (- num2 1) counter counter2)
		)





	)

)
(defun primecrawler (num1 num2 stream)  
	(cond 
		((= num1 (+ num2 1)) nil)
		((isPrime num1 (- num1 1 ))
			;;in case the number is a prime number
			(cons 
			
				(cons
					(format stream "~S " num1 ) 
					(format stream "is prime ~%")
				 );
				(primecrawler (+ num1 1) num2 stream)
			) 
		)
		((is_semiPrime num1 (- num1 1 ) 0 0)
			;;in case the number is a semi prime number
			(cons 
				(cons 
					(format stream "~S " num1 )
					(format stream "is semi prime ~%")
				)
				(primecrawler (+ num1 1) num2 stream )
			) 
		)
		(t
			;; if the number is neither prime nor semi prime 
			(primecrawler (+ num1 1) num2 stream ) 
			
		)

	)



)
(defun write_to_file ( num1 num2)

(with-open-file (stream "primedistribution.txt"
									:direction :output 
									:if-exists :supersede
									:if-does-not-exist :create)
 	(primecrawler num1 num2 stream)
 	
 )

)

 
 (defun main ()

 	(setq input_file "boundries.txt")
	(setq list1 (car(read_from_file input_file)) )
	(if (null (car list1)) 

	(cons 
		(print "the input file is empty ")  ;;empty input file case
		(return-from main nil) )

	)(if (null (car(cdr list1)))          ;;  insufficient input file 

	(cons 
		(print "the input file has only one element ")
		(return-from main nil) )

	)
	(setq num1 (min (car list1) (car(cdr list1))))
	(setq num2 (max (car list1) (car(cdr list1))))
	(write_to_file num1 num2)
	

 )
 (main)