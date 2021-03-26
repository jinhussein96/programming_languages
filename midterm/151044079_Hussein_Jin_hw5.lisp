;clause is prolog fact like this 
;man(X,Y):-name(X),surname(Y)
;represented as list (("man",("X","Y")), (("name"("X")),("surname"("Y"))).
;first element is left predicate.
;second element is  list of right side predicates.
;string which starts with UPPERCASE char is variable.
;knowledge is list of clauses.

;returns T (true) if arg string 
;and starting with upper case char
(defun is-var (arg) 
	(and (stringp arg) 
		(upper-case-p (char arg 0))))

;returns T (true) if arg is number or string 
;starting with lowercase char
(defun is-not-var (arg) 
	(or (numberp arg)
		(and (stringp arg) 
			(lower-case-p (char arg 0)))))

;checks whether predicate args parg like ("john","smith") 
;matches clause args carg like (X,Y)
(defun match-arg (parg carg) 
	(or (and (is-not-var  parg) (is-not-var carg) (equal parg carg))
		(and  (is-var parg) (is-not-var carg))
		(and  (is-var parg) (is-var carg))
		(and (is-not-var parg) (is-var carg)) ))

;checks whether predicate like man("john","smith") matches clause in knowledge
;first it checks predicate name and then checks args matching
(defun matches-args (predicate left-side-clause)
	(let ((predicate-name (first predicate))
		  (clause-name (first left-side-clause))
		  (predicate-args (second predicate))
		  (clause-args (second left-side-clause)))
		(and 
			(equal predicate-name clause-name)
			(equal (length predicate-args) (length clause-args))
			 (every #'match-arg  predicate-args  clause-args ))))

;match-vars checks len, vars, values

(defun fact-values (args values)
	(if (every #'match-arg args values )
		(mapcar #'(lambda (arg value) 
			(list arg (list value))) args values)
		(mapcar #'(lambda (arg) (list arg nil)) args)))

(defun replace-arg (child-arg parent-args parent-values)
	(cond 
		((not parent-args) nil)
		((is-not-var child-arg) child-arg)
		((and (is-var child-arg) (equal child-arg (first parent-args)) (first parent-values)))
		((and (is-var (first parent-values))) (first parent-args))
		(t (replace-arg child-arg (rest parent-args) (rest parent-args)))))

(defun replace-args (child args entry-args)
	(list (first child) 
		(mapcar #'(lambda (child-arg)
			(replace-arg child-arg args entry-args))  (second child))))

(defun get-by-key (key values)
	(cond
	 	((not values) nil)
	 	((equal key (first (first values))) 
			(second (first values)))
	 	(T (get-by-key key (rest values)))))

(defun intersect (list1 list2)
	(remove-duplicates 
		(reduce #'(lambda (acc item1) 
			(if (find-if #'(lambda (item2) (equal item1 item2)) list2) 
				(cons item1 acc)  acc))
	 	list1 :initial-value nil) :test #'equal))

(defun join-value (key new all)
	(cond 
		((null new) all)
		((null all) (list key new)) 
		(T (list key (intersect (second all) new)))))

(defun validate (args) 
	(if (every #'(lambda (arg) 
		(or (and  (listp arg) (not (null (second arg))))
			(not (null arg)))) args)
	 	args nil))

(defun every-depends-has-values (children values)
	(and (equal (length children) (length values))
		(every #'(lambda (child value) 
			(equal (length (second child)) (length value)))
		children values)))
	
(defun join-child-values (args entry-args children values)
	(validate 
		(if (every-depends-has-values children values)
			(mapcar #'(lambda (key entry-key) 
				(if (is-var entry-key)
					(reduce #'(lambda (acc val) 
						 (join-value key  (get-by-key key val) acc ))
					values :initial-value nil)
					(list key (list entry-key)) ))  
				args entry-args )
			;else
			(mapcar #'(lambda (key) (list key nil)) args))))


(defun collect-values (args values entry knowledge)
	(cond 
		((and (not (second entry )) (equal (length args) (length  values) ))
			 (fact-values args values ))
		((equal (length args) (length values))
			(join-child-values args (second (first entry)) (second entry) (mapcar #'(lambda (child) 
				(collect-and-join (replace-args  child (second (first entry))  args ) knowledge)) (second entry)) ))
		(T nil)))


(defun add-value (key value data)
	(cond 
		((null  data ) (list (list key value)))
		((equal key (first (first data))) 
			(cons (list key 
				(remove-duplicates 
					(concatenate 'list value (second (first data))) 
					:test #'equal)) 
			(rest data)))
			
		(T (cons (first data) (add-value key value (rest data))))))

(defun add-values (values data)
	(reduce #'(lambda (acc value) 
		(add-value (first value) (second value) acc)) 
	values :initial-value data))

(defun join-entries-values (values)
	(reduce #'(lambda (acc value) 
		(if (null value) acc
			(add-values value acc))) 
	values :initial-value nil))

(defun select-matched (predicate knowledge)
	(remove-if #'(lambda (entry) 
		 (not (matches-args  predicate  (first entry)) )) knowledge))

(defun collect-and-join (predicate knowledge)
	(let ((entries (select-matched predicate knowledge)))
		(if (and (null (second predicate)) (not (null entries))) (list (list (list "yes" (list "yes"))))
			(join-entries-values
				(reduce #'(lambda (acc entry) 
					(let ((values 
						(collect-values 
							 (second predicate)
							(second (first entry)) entry knowledge)))
					(if values (cons values acc) acc)))  entries :initial-value nil)))))

(defun refine (data)
	(cond
		((null data) nil)
		((null (second (first (first data) ))) nil) 
		(T (cons (first data) (refine (rest data) )))))

(defun collect (predicate knowledge)
	(refine (let ((entries (select-matched predicate knowledge)))
		(if (and (null (second predicate)) (not (null entries))) (list (list (list "yes" (list "yes"))))
			(reduce #'(lambda (acc entry) 
			(let ((values 
				(collect-values 
					 (second predicate)
					(second (first entry)) entry knowledge)))
			(if values (cons values acc) acc)))  entries :initial-value nil)))))
(defun list-to-comma-string (lst)
    (format nil "~{~A,~}" lst))

(defun list-to-string (lst)
    (format nil "~{~A~}" lst))

(defun result-to-string (result)
	(cond ((null result) "no")
		  ((typep result 'boolean) "yes")
		  ((every #'(lambda (key) (is-not-var (first key) )) (first result)) "yes")
		  (T (list-to-comma-string result))
		))

(defun query-to-string (query result) 
	(concat (list "?- " (if (stringp (first query)) 
		(list-to-string query) 
		(list-to-comma-string (mapcar #'list-to-string query)))
		" => " (result-to-string result) )))

(defun query-has-vars (query)
	(some #'(lambda (arg) (is-var arg)) (second query)) )
;the function calls proof function to query 
(defun process-query (query knowledge)
		(let ((result 
			(cond 
				((null query) T)
				((stringp (first query)) (collect query knowledge))
				(T (every #'(lambda (predicate) (proof predicate knowledge)) query)) )))
			(query-to-string query result)))
		
;the function takes first elemet of input-list
;and if item is query if tries to proof and 
;calls process-input on rest of input-list
;if item is clause (fact definition) it adds it to knowledge
;if end of input-list reached it return (knowledge,output)
(defun process-input (input-list knowledge output)
	(let ((head (first input-list))
		  (tail (rest input-list)))
	(cond 
		((and (not head) tail) (process-input tail knowledge 
				(cons (process-query head knowledge) output)))
		((equal (length input-list) 0) (list knowledge output))
		((first head) (process-input tail (cons head knowledge) output))
		(t (process-input tail knowledge 
				(cons (process-query (second head) knowledge) output))))))

;concatenates list of string
(defun concat (list)
  (reduce (lambda (a b) (concatenate 'string a b)) list))

;recursively reads input stream and creates list of lines
(defun read-lines (stream)
	(let ((line (read-line stream nil))) 
		(if line 
			(cons line (read-lines stream))
			nil)))

;reads input-file and converts into lisp list
(defun parse-input (input-file) 
	(with-open-file (stream input-file )
		 (read-from-string (concat (read-lines stream)))))

;saves the output and knowlege
;output param is list (knowledge,output)
(defun save-output (output-filename knowledge-filename output )
	(or 
		(with-open-file (stream output-filename :direction :output) 
			(format stream "~{~a~^~%~}" (second output)))
	 	(with-open-file (stream knowledge-filename :direction :output) 
	 		(format stream "~{~a~^~%~}" (first output)))
	 	T))
	
; 1 reads input.txt and converts it to list structure
; 2 processes all data
; 3 writes output and knowledge to files "output.txt" "knowledge.txt" 
(defun main ()
	(save-output "output.txt" "knowledge.txt" 
		(process-input 
			(parse-input "input.txt") 
			nil nil)))


(main)

