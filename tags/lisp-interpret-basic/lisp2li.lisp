;; Prend une expression lisp en entree et 
;; retourne une expression ecrite dans le langage intermediaire
(defun lisp2li (expr env)
  ;; expr est un atome ;;
  (if (atom expr)
      ;; cas des literaux
      (if (constantp expr) 
	  (cons :LIT expr)
	;; cas des variables, id commence a 1
	(let ((pos (+ (position expr env) 1))) 
	  (if pos
	      (cons :VAR pos)
	    (warn "La variable ~s n'existe pas" expr))))
    ;; expr n'est pas un atome ;;
    (let ((fun (first expr)) (args (rest expr)))
      (cond

       ; cas ou expr n'est pas un symbole
       ((not (symbolp fun))
	;; cas des lambdas
	(if (and (consp fun) (eq 'lambda (first fun))) 
	    (warn "NYI ~s" (first fun))
	  (warn "~s n'est pas une lambda" fun)))

       ;; Cas des fonctions creer
       ((get-defun fun)
	`(:MCALL ,fun ,@(map-lisp2li args env))) 

       ; si fun n'a pas de valeur fonctionnelle
       ((not (fboundp fun)) 
	`(:UNKNOWN ,expr ,env))

       ;; si fun est une forme speciale
       ((special-form-p fun) 
	(case fun
	      ('quote `(:LIT ,@(first args)))
	      ('if `(:IF
		     ,(lisp2li (first args) env)
		     ,(lisp2li (second args) env)
		     ,(lisp2li (third args) env)))
	      ('progn (cond ((null args) (list :LIT))
			    ((null (rest args)) (lisp2li (first args) env))
			    (t (cons :PROGN (map-lisp2li args env)))))
	      (t (warn "NYI special form ~s" fun))))

       ;; si fun est une macro
	((macro-function fun) 
	 (case fun
	       ('defun `(:CALL set-defun (:LIT . ,(first args))
			       (:LIT :LAMBDA ,(+ 1 (length (second args)))
				     ,(count-lvars (third args))
				     ,(lisp2li (third args) (second args)))))
	       ('setf `(:SET-VAR ,(cdr (lisp2li (car args) env)) ,(lisp2li (cadr args) env)))
	       (t (lisp2li (macroexpand-1 expr) env))))
	
	;; sinon fun est une "vraie" fonction
	(t `(:CALL ,fun ,@(map-lisp2li args env)))))))

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
