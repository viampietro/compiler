(require "./lisp2li.lisp")
(require "./eval-li.lisp")

;; Compare expr-li avec le retour de la fonction
;; lisp2li qui prend en args expr-lisp et env
(defun check-lisp2li (expr-lisp env expr-li)
  (progn
    (format t "Traduction de ~s~%" expr-lisp)
    (if (check-li expr-li)
	(let ((expr-li-prod (lisp2li expr-lisp env)))
	  (if(not (equal expr-li expr-li-prod))
	      (warn "Expected ~s Received ~s~%~%" expr-li expr-li-prod)
	    (format t "Equals ~s~%~%" expr-li expr-li-prod)))
      (warn "L'expression LI ~s passée en argument n'est pas correcte !~%" expr-li))))

;; Prend une expr en li et verifie sa forme
(defun check-li (expr) 			
  (if (atom expr)
      (warn "Une expr en LI doit être une liste")
    (eCase (first expr)
	   (:LIT T)
	   (:VAR (if (numberp (rest expr))
		     T
		   (warn ":VAR doit être suivi d'un indice numérique")))
	   (:SET-VAR (if (= 2 (length (rest expr)))
			 T
		       (warn ":SET-VAR number expr-li")))
	   (:PROGN (if (map-check-li (rest (rest expr)))
		       T
		     (warn "~s est mal formée" expr)))
	   (:IF (if (= (length (rest expr)) 3)
		    (map-check-li (rest expr))
		  (warn "Un :IF doit être suivi de 3 arguments")))
	   (:UNKNOWN (if (= (length (rest expr)) 2)
			 T
		       (warn "~s est mal formée" expr)))
	   (:CALL (if (map-check-li (rest (rest expr)))
		      T
		    (warn "~s est mal formée" expr)))
	   (:MCALL (if (map-check-li (rest (rest expr)))
		       T
		     (warn "~s est mal formée" expr))))))

;; Appelle check-li recursivement sur 
;; la liste d'expressions lexpr
(defun map-check-li (lexpr)
  (if (atom lexpr)
      T
    (and (check-li (first lexpr)) (map-check-li (rest lexpr)))))

;; Prend une expression LI (expr-li), un environnement (env) et la valeur de retour
;; escomptee (return-value) apres l'application de eval-li sur expr-li et env
(defun check-eval-li (expr-li env return-value)
  (if (check-li expr-li)
      (let ((el-return-value (eval-li expr-li env)))
	(if (eql el-return-value return-value)
	    (format t "EVAL ~s ~s => ~s~%~%" expr-li env return-value)
	  (warn "EVAL ~s ~s => ~s not ~s~%" expr-li env el-return-value return-value)))
    (warn "L'expression LI ~s passée en argument n'est pas correcte !~%" expr-li)))

(defun check-count-lvars (lexpr count)
  (let ((count-res (count-lvars lexpr)))
    (if (= count count-res)
	(format t "Equal counts ~s and ~s~%" count count-res)
      (warn "Expected ~s, received ~s" count count-res))))

;; Prend une expression lisp, un environnement
;; et une valeur de retour attendue en arguments.
;; Vérifie que l'interpretation de expr
;; (composition de lisp2li et eval-li) donne bien return-value.
(defun check-interpret (expr env return-value)
  (let ((result (interpret expr env)))
    (if (not (equal result return-value))
	(warn "EVAL ~s ~s => ~s not ~s" expr env result return-value)    
      (format t "EVAL ~s ~s => ~s~%" expr env result))))

;; TEST avec check-lisp2li
(defun test-lisp2li ()
  (check-lisp2li '(defun fun-lvars (x y)
		    (let ((a 1) (b 2))
		      (+ x y a b)))
		 '()
		 '(:CALL set-defun
			 (:LIT . fun-lvars)
			 (:LIT :LAMBDA 2 5 (:PROGN
					    (:SET-VAR 3 (:LIT . 1))
					    (:SET-VAR 4 (:LIT . 2))
					    (:CALL + (:VAR . 1) (:VAR . 2) (:VAR . 3) (:VAR . 4)))))))


(defun test-count-lvars ()
  (progn
    (check-count-lvars '(let ((x 2) (y 6))) 2)
    (check-count-lvars '(if (= 2 2)
			    (let ((x 2) (y 6))
			      (+ x y))
			  ()) 2)))

;; TEST avec check-eval-li
(defun test-eval-li ()
  (progn
    (eval-li '(:CALL set-defun
		     (:LIT . fun-lvars)
		     (:LIT :LAMBDA 2 5 (:PROGN
					(:SET-VAR 3 (:LIT . 1))
					(:SET-VAR 4 (:LIT . 2))
					(:CALL + (:VAR . 1) (:VAR . 2) (:VAR . 3) (:VAR . 4))))) '#(nil))
    (eval-li '(:MCALL fun-lvars (:LIT . 1) (:LIT . 3)) '#(nil))))

;; TEST Composition de lisp2li et eval-li
(defun interpret (expr env)
  (eval-li (lisp2li expr env) '#(nil)))


(defun test-interpret ()
  (progn 
    ;; Appels simple sur fibo meta-evaluee
    (interpret '(defun fibo (n)
		  (if (<= n 1)
		      1
		    (+ (fibo (- n 1))
		       (fibo (- n 2))))) '())
    (format t "MEVAL ~s => ~s~%" '(fibo 20) (time (interpret '(fibo 20) '())))

    ;; Appels simple sur fibo avec eval
    ;; defun de fibo pour l'evaluateur lisp
    (defun fibo (n)
      (if (<= n 1)
	  1
	(+ (fibo (- n 1))
	   (fibo (- n 2)))))
    (format t "EVAL ~s => ~s~%" '(fibo 20) (time (fibo 20)))
    
    ;; Appels croises sur fibo1 et fibo2
    (interpret '(defun fibo1 (n)
		  (if (<= n 1)
		      1
		    (+ (fibo2 (- n 1))
		       (fibo2 (- n 2))))) '())
    (interpret '(defun fibo2 (n)
		  (if (<= n 1)
		      1
		    (+ (fibo1 (- n 1))
		       (fibo1 (- n 2))))) '())
    (format t "EVAL ~s => ~s~%" '(fibo1 3) (interpret '(fibo1 3) '()))
    (format t "EVAL ~s => ~s~%" '(fibo2 3) (interpret '(fibo2 3) '()))

    (interpret '(interpret '(+ 1 2) '()) '())
    (format t "~s~%" (lisp2li '(eval-li (:LIT . 1) #(nil)) '()))
    ;; Normalement cette ligne génère une erreur car les mots cles ne sont pas traites
    (eval-li (lisp2li '(eval-li (:LIT . 1) #(nil)) '()) '#(nil))))

(defun test-let2setf ()
  (progn
    (let2setf '((x 1) (y 2) (z 3) (b 5)))))
