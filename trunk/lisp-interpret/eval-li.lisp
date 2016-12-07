;; Prend une expression LI et un environnement associe en argument
;; et retourne le resultat de l'evaluation de l'expression.
(defun eval-li (expr env)
  (if (atom expr)
      (error "~s n'est pas une expression LI" expr)
    (ecase (car expr)
	   (:LIT (cdr expr))
	   (:VAR (aref env (cdr expr)))
	   (:SET-VAR (setf (aref env (cadr expr)) (eval-li (caddr expr) env)))
	   (:IF (if (eval-li (second expr) env)
		    (eval-li (third expr) env)
		  (eval-li (fourth expr) env)))
	   (:CALL (apply (symbol-function (second expr)) (map-eval-li (cddr expr) env)))
	   (:MCALL (let ((fun (get-defun (second expr))))
		     (eval-li (fourth fun)
			      (make-eval-li-env
			       (map-eval-li (cddr expr) env) (third fun)
			       (- (third fun) (second fun) 1)))))
	   (:PROGN (map-eval-li-progn (cdr expr) env))
	   (:UNKNOWN (let ((nexpr (lisp2li (second expr) (third expr))))
		       (if (eq (car nexpr) :UNKNOWN)
			   (error "UNKNOWN ~s" (cdr expr))
			 (eval-li (displace expr nexpr) env)))))))

;; Prend une liste d'expressions LI en argument
;; et retourne la liste des valeurs de ces expressions
(defun map-eval-li (lexpr env)
  (if (atom lexpr)
      lexpr
    `(,(eval-li (first lexpr) env) ,@(map-eval-li (rest lexpr) env))))

;; Prend une liste d'expressions evaluees en parametre et un taille
;; de tableau pour remplir un tableau nouvellement creer avec lexpr.
(defun make-eval-li-env (lexpr array-size nb-lvars)
    (make-array array-size
		:initial-contents `(nil ,@lexpr ,@(make-list nb-lvars))))

;; Prend une liste d'expressions LI en argument, evalue chaque expression
;; et retourne le resultat de la derniere expression evaluee.
(defun map-eval-li-progn (lexpr env)
  (if (null (rest lexpr))
      (eval-li (first lexpr) env)
    (progn
      (eval-li (first lexpr) env)
      (map-eval-li-progn (rest lexpr) env))))

;; Affectation de la valeur de nexpr dans expr.
(defun displace (expr nexpr)
  ;; progn implicite
  (setf (car expr) (car nexpr)
	(cdr expr) (cdr nexpr))
  expr)
	
