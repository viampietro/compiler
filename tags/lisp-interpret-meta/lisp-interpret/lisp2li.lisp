;; Prend une expression lisp en entree et 
;; retourne une expression ecrite dans le langage intermediaire
(defun lisp2li (expr env)
  (let ((fun nil)
	(args nil)
	(lsetf nil))
    
    ;; expr est un atome ;;
    (if (atom expr)
	;; cas des literaux
	(if (constantp expr) 
	    (cons :LIT expr)
	  ;; cas des variables, id commence a 1
	  
	  (if (position expr env)
	      (cons :VAR (+ (position expr env) 1))
	    (warn "La variable ~s n'existe pas" expr)))
      ;; expr n'est pas un atome ;;
      (progn
	(setf fun (first expr))
	(setf args (rest expr))

	(cond

	 ;; cas ou expr n'est pas un symbole
	 ((not (symbolp fun))
	  ;; cas des lambdas
	  (if (and (consp fun) (eq 'lambda (first fun))) 
	      (warn "NYI ~s" (first fun))
	    (warn "~s n'est pas une lambda" fun)))

	 ;; Cas des fonctions creer
	 ((get-defun fun)
	  `(:MCALL ,fun ,@(map-lisp2li args env))) 

	 ;; si fun n'a pas de valeur fonctionnelle
	 ((not (fboundp fun)) 
	  `(:UNKNOWN ,expr ,env))

	 ;; si fun est une macro
	 ((macro-function fun) 
	  (case fun
		(setf `(:SET-VAR ,(cdr (lisp2li (car args) env)) ,(lisp2li (cadr args) env)))
		(cond (lisp2li (cond2if args) env))
		(case (lisp2li (case2if (cdr args) (car args)) env))
		(defun
		    ;; si la fonction contient des vars locales
		    (if (eq 'let (car (third args))) 
			(progn
			  (setf lsetf (let2setf (cadr (third args))))
			  `(:CALL set-defun (:LIT . ,(first args))
				  (:LIT :LAMBDA
					,(length (second args)) ;; nombre d'args
					,(+ 1 (length (second args)) (length (car lsetf))) ;; nombre d'args + nb lvars + 1
					,(lisp2li `(progn ,@(cadr lsetf) ,(caddr (third args))) `(,@(second args) . ,(car lsetf))))))
		      `(:CALL set-defun (:LIT . ,(first args))
			      (:LIT :LAMBDA
				    ,(length (second args)) ;; nombre d'args
				    ,(+ 1 (length (second args))) ;; nombre d'args + nb lvars + 1
				    ,(lisp2li (third args) (second args))))))
		(t (lisp2li (macroexpand-1 expr) env))))

	 ;; si fun est une forme speciale
	 ((special-form-p fun) 
	  (case fun
		(quote `(:LIT ,@(first args)))
		(if `(:IF
		      ,(lisp2li (first args) env)
		      ,(lisp2li (second args) env)
		      ,(lisp2li (third args) env)))
		(progn (cond ((null args) (list :LIT))
			     ((null (rest args)) (lisp2li (first args) env))
			     (t (cons :PROGN (map-lisp2li args env)))))
		(t (warn "NYI special form ~s" fun))))
	 
	 ;; sinon fun est une "vraie" fonction
	 (t `(:CALL ,fun ,@(map-lisp2li args env))))))))

;; Applique la fonction lisp2li sur chaque expression
;; de la liste lexpr
(defun map-lisp2li (lexpr env)
  (if (atom lexpr)
      ()
    `(,(lisp2li (first lexpr) env) ,@(map-lisp2li (rest lexpr) env))))

;; retourne la valeur fonctionnelle
;; de symb, contenue dans la propriete :defun
(defun get-defun (symb)
  (get symb :defun))

(defun set-defun (symb fun)
  (setf (get symb :defun) fun))

;; prend une expression lisp en arguments et compte le nombre
;; de variables locales (parcours de l'arbre syntaxique)
(defun count-lvars (expr)
  (if (atom expr)
      0
    (if (equal 'let (car expr))
	(+ (length (cadr expr)) (count-lvars (cddr expr))) 
      (+ (count-lvars (car expr)) (count-lvars (cdr expr))))))

;; prend en argument une liste d'expressions lisp correspondant
;; a la declaration et initialisation de vars locales dans un let.
;; retourne la liste des vars locales declarees ainsi qu'une liste d'operations
;; (setf <var> <value>).
(defun let2setf (lexpr)
  (let ((ret nil)
	(var nil)
	(value nil))
    (if (atom lexpr)
	lexpr
      (progn
	(setf ret (let2setf (cdr lexpr)))
	(setf var (caar lexpr))
	(setf value (cadar lexpr))
	`((,var ,@(car ret))
	  ((setf ,var ,value) ,@(cadr ret)))))))

;; prend une liste d'expressions lisp qui correspond a la liste
;; d'expressions suivant un cond, et la transforme en if imbriques
(defun cond2if (lexpr)
  (let ((next-cond (car lexpr)))
    (if (equal t (car next-cond))
	(cadr next-cond)	 
      `(if ,(car next-cond)
	   ,@(cdr next-cond)
	 ,(cond2if (cdr lexpr))))))

;; prend une liste d'expressions lisp qui correspond a la liste
;; d'expressions suivant la forme speciale case, et la transforme en if imbriques
(defun case2if (lexpr key)
  (let ((next-case (car lexpr)))
    (if (equal t (car next-case))
	(cadr next-case)
      ;; si la clause commence par un atome (ex : (:IF ...))
      (if (atom (car next-case))
	  `(if (eql ,key ',(car next-case))
	       ,@(cdr next-case)
	     ,(case2if (cdr lexpr) key))
	;; si la clause est une liste de cles
	`(if (member ,key ',(car next-case))
	     ,@(cdr next-case)
	   ,(case2if (cdr lexpr) key))))))
