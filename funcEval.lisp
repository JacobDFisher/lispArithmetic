;; fix representation
;; rather than having (* (/ 1 4) 5 (/ 1 8) 7)
;; have (/ (* 5 7) (* 4 8))
;; same with minus
;; in add and multiply, keep 2 lists, one for +'s, and one for -'s
;; fix lispify-multiply to allow negatives, parse in "(-" before subexpression

(defun math-tokenizer (func-string)
  (if (> (length func-string) 0)
      (let ((next-non-whitespace (position-if-not #'(lambda (c) (char= c #\Space)) func-string)))
	(if next-non-whitespace
	    (if (find (char func-string next-non-whitespace) "+-*/^=()")
		(cons (subseq func-string next-non-whitespace (+ 1 next-non-whitespace))
		      (math-tokenizer (subseq func-string (+ 1 next-non-whitespace))))
		(cons (subseq func-string
			      next-non-whitespace
			      (or
			       (position-if #'(lambda (c) (find c "+-*/^=() ")) func-string :START next-non-whitespace)
			       (length func-string)))
		      (math-tokenizer
		       (subseq func-string
			       (or
				(position-if #'(lambda (c) (find c "+-*/^=() ")) func-string :START next-non-whitespace)
				(length func-string))))))))))

(defun lispify-wrap (func operator)
  (if (> (length (cadr func)) 1)
      `(,(car func) (,operator ,@(cadr func)))
      `(,(car func) ,@(cadr func))))

;; (defun exponent-wrapped (list)
;;   (if (cdadr list)
;;       (let ((subexpression (exponent-wrap `(,(car list) ,(cdadr list)))))
;; 	`(,(car list) ("EXPT" ,(cadr subexpression) ,(caadr list))))
;;       `(,(car list) ,@(cadr list))))

;; (defun exponent-wrap (list)
;;   (exponent-wrapped `(,(car list) ,(reverse (cadr list)))))

(defun delistify (list)
  (if (> (length list) 1)
      list
      (car list)))

(defun lispify-paren (string-list)
  (if (string= (car string-list) "(")
      (let* ((subexpression (lispify-wrap (lispify-add (cdr string-list)) "+")) (next (+ 1 (car subexpression))))
	(if (string= (nth next string-list) ")")
	    `(,(+ 1 next) ,@(cdr subexpression))))
      `(1 ,(car string-list))))

(defun lispify-exponent (string-list)
  (let* ((subexpression (lispify-paren string-list)) (next (car subexpression)))
    (if (string= (nth next string-list) "^")
	(let ((next-expression (lispify-negative (nthcdr (+ 1 next) string-list))))
	  `(,(+ (car next-expression) next 1) ("EXPT" ,(cadr subexpression) ,(cadr next-expression))))
	subexpression)))

(defun lispify-negative (string-list)
  (let* ((negative (string= (car string-list) "-"))
	 (subexpression (if negative
			    (lispify-negative (cdr string-list))
			    (lispify-exponent string-list))))
    (if negative
	`(,(+ 1 (car subexpression)) ("-" ,(cadr subexpression)))
	subexpression)))

(defun lispify-multiply (string-list)
  (let* ((subexpression (lispify-negative string-list)) (next (car subexpression)))
    (if (string= (nth next string-list) "*")
	(let ((next-expression (lispify-multiply (nthcdr (+ 1 next) string-list))))
	  ;; (format t "*: ~a~%~a~%~%" subexpression next-expression)
	  `(,(+ (car next-expression) next 1) (,(cadr subexpression) ,@(cadr next-expression))))
	(if (string= (nth next string-list) "/")
	    (let ((next-expression (lispify-multiply (nthcdr (+ 1 next) string-list))))
	      ;; (format t "/: ~a~%~a~%~%" subexpression next-expression) 
	      `(,(+ (car next-expression) next 1) (,(cadr subexpression) ("/" "1" ,(caadr next-expression)) ,@(cdadr next-expression))))
	      `(,(car subexpression) ,(cdr subexpression))))))

(defun lispify-add (string-list)
  (let* ((subexpression (lispify-wrap (lispify-multiply string-list) "*")) (next (car subexpression)))
    (if (string= (nth next string-list) "+")
	(let ((next-expression (lispify-add (nthcdr (+ 1 next) string-list))))
	  ;; (format t "*: ~a~%~a~%~%" subexpression next-expression)
	  `(,(+ (car next-expression) next 1) (,(cadr subexpression) ,@(cadr next-expression))))
	(if (string= (nth next string-list) "-")
	    (let ((next-expression (lispify-add (nthcdr (+ 1 next) string-list))))
	      ;; (format t "/: ~a~%~a~%~%" subexpression next-expression) 
	      `(,(+ (car next-expression) next 1) (,(cadr subexpression) ("-" ,(caadr next-expression)) ,@(cdadr next-expression))))
	      `(,(car subexpression) ,(cdr subexpression))))))

(defun lispify-equal (string-list)
  (let* ((subexpression (lispify-wrap (lispify-add string-list) "+")) (next (car subexpression)))
    (if (string= (nth next string-list) "=")
	(let* ((next-expression (lispify-equal (nthcdr (+ 1 next) string-list))))
	  `(,(+ (car next-expression) next 1) (,(cadr subexpression) ,@(cadr next-expression))))
	`(,(car subexpression) ,(cdr subexpression)))))

(defun lispify-expression (string-list)
  (cadr (lispify-wrap (lispify-equal string-list) "=")))

(defun math-evaluate (expression)
  (if (stringp expression)
      (parse-integer expression)
      (apply (intern (car expression)) (mapcar #'math-evaluate (cdr expression)))))

(defun math-evaluate-string (string)
  (math-evaluate (lispify-expression (math-tokenizer string))))

(defun generate-paren ()
  (let ((check (random 20)))
    (if (< check 3)
	(let ((subexpression (generate-add)))
	  `(,(concatenate 'string "(" (car subexpression) ")") ,(cadr subexpression)))
	(let ((num (random 50)))
	  `(,(write-to-string num) ,num)))))

(defun generate-exponent ()
  (let ((check (random 20)))
    (if (< check 4)
	(let ((sub1 (generate-paren)) (sub2 (+ 1 (random 5))))
	  `(,(concatenate 'string (car sub1) "^" (write-to-string sub2)) ,(expt (cadr sub1) sub2))))
    (generate-paren)))

(defun generate-multiply ()
  (let ((check (random 20)))
    (if (< check 7)
	(let ((sub1 (generate-exponent)) (sub2 (generate-exponent)))
	  `(,(concatenate 'string (car sub1) "*" (car sub2)) ,(* (cadr sub1) (cadr sub2))))
	(if (< check 10)
	    (let ((sub1 (generate-exponent)) (sub2 (generate-exponent)))
	      (if (/= (cadr sub2) 0)
		  `(,(concatenate 'string (car sub1) "/" (car sub2)) ,(/ (cadr sub1) (cadr sub2)))
		  sub1))
	    (generate-exponent)))))
	    

(defun generate-add ()
  (let ((check (random 20)))
    (if (< check 7)
	(let ((sub1 (generate-multiply)) (sub2 (generate-multiply)))
	  `(,(concatenate 'string (car sub1) "+" (car sub2)) ,(+ (cadr sub1) (cadr sub2))))
	(if (< check 10)
	    (let ((sub1 (generate-multiply)) (sub2 (generate-multiply)))
	      `(,(concatenate 'string (car sub1) "-" (car sub2)) ,(- (cadr sub1) (cadr sub2))))
	    (generate-multiply)))))


(defun math-generate-string-and-answer ()
  (let ((sub1 (generate-add)))
    (if (> (length (math-tokenizer (car sub1))) 1)
	sub1
	(let ((sub2 (generate-add)))
	  `(,(concatenate 'string (car sub1) "+" (car sub2)) ,(+ (cadr sub1) (cadr sub2)))))))

(defun math-generate-strings (num)
  (format t "~a~%" (car (math-generate-string-and-answer)))
  (if (> num 0)
      (math-generate-strings (- num 1))))
      

(defun math-test-func (num)
  (let* ((func (math-generate-string-and-answer)) (incorrect (if (/= (cadr func) (math-evaluate-string (car func))) `(,(car func)))))
    (if (> num 0)
	(let ((subexpression (math-test-func (- num 1))))
	  `(,@incorrect ,@subexpression))
	incorrect)))

(defun math-input-strings ()
  (loop (let ((input (read-line *query-io*)))
	  (if (> (length input) 0)
	      (format t "~a~%" (math-evaluate-string input))
	      (return)))))


	

;; Parse-float tomorrow
