;; prend une expression LI en arguments ainsi qu'un nombre de params
;; retourne une liste d'expressions vm.
(defun li2vm (expr nbparam)
  (ecase (car expr)
	 (:LIT `((:CONST . ,(cdr expr))))
	 (:VAR `((:VAR . ,(cdr expr))))
	 (:SET-VAR `(,@(li2vm (caddr expr) nbparam)
		     (:SET-VAR . ,(cadr expr))))
	 (:CALL (let ((fun (second expr)) (args (cddr expr)))
		  `(,@(map-li2vm args nbparam) (:CONST . ,(length args)) (:CALL ,fun))))
	 (:MCALL (let ((fun (second expr)) (args (cddr expr)))
		   `(,@(map-li2vm args nbparam) (:CONST . ,(length args)) (:CALL ,fun))))
	 (:UNKNOWN (let ((nexpr (lisp2li (second expr) (third expr))))
		     (if (eq (car nexpr) :UNKNOWN)
			 (error "UNKNOWN ~s" (cdr expr))
		       (li2vm (displace expr nexpr) (length (third expr))))))
	 (:IF (let ((bloc-alors (li2vm (third expr) nbparam))
		    (bloc-sinon (li2vm (fourth expr) nbparam)))
		`(,@(li2vm (second expr) nbparam)
		  (:SKIPNIL . ,(+ 1 (length bloc-alors)))
		  ,@bloc-alors
		  (:SKIP . ,(length bloc-sinon))
		  ,@bloc-sinon)))
	 (:PROGN (map-li2vm (cdr expr) nbparam))))

(defun fun2vm (fun)
  (let ((fun-value (get-defun fun)))
    (if (null fun-value)
	(warn "~s n'a pas de valeur fonctionnelle" fun)
    ;; on definit une etiquette pour la fonction
      `((:LABEL ,fun)
	(:STACK ,(third fun-value)) ;; nombre de vars locales
	,@(li2vm (fourth fun-value) (second fun-value))
	(:RTN)))))

;; prend une liste d'expressions LI en param
;; et retourne cette meme liste traduite en expressions
;; du langage vm
(defun map-li2vm (lexpr nbparam)
  (if (atom lexpr)
      lexpr
    `(,@(li2vm (first lexpr) nbparam) ,@(map-li2vm (rest lexpr) nbparam))))


