;; prend une expression LI en arguments ainsi qu'un nombre de params
;; retourne une liste d'expressions vm.
(defun li2vm (expr nbparam)
  (let ((fun nil)
	(args nil)
	(nexpr nil)
	(bloc-alors nil)
	(bloc-sinon nil)
	(loop-body nil)
	(loop-stop-cond nil))
    (case (car expr)
	   (:LIT `((:CONST . ,(cdr expr))))
	   (:VAR `((:VAR . ,(cdr expr))))
	   (:SET-VAR `(,@(li2vm (caddr expr) nbparam)
		       (:SET-VAR . ,(cadr expr))))
	   (:CALL (progn
		    (setf fun (second expr))
		    (setf args (cddr expr))
		    `(,@(map-li2vm args nbparam) (:CONST . ,(length args)) (:CALL ,fun))))
	   (:MCALL (progn
		     (setf fun (second expr))
		     (setf args (cddr expr))
		     `(,@(map-li2vm args nbparam) (:CONST . ,(length args)) (:CALL ,fun))))
	   (:MCALLT (progn
		      (setf fun (second expr))
		      (setf args (cddr expr))
		      `(,@(map-li2vm args nbparam) ,@(gen-setvars (length args)) (:UNSTACK . ,(length args)) (:CALLT ,fun))))
	   (:UNKNOWN (progn
		       (setf nexpr (lisp2li (second expr) (third expr)))
		       (if (eq (car nexpr) :UNKNOWN)
			   (error "UNKNOWN ~s" (cdr expr))
			 (li2vm (displace expr nexpr) (length (third expr))))))
	   (:IF (progn
		  (setf bloc-alors (li2vm (third expr) nbparam))
		  (setf bloc-sinon (li2vm (fourth expr) nbparam))
		  `(,@(li2vm (second expr) nbparam)
		    (:SKIPNIL . ,(+ 1 (length bloc-alors)))
		    ,@bloc-alors
		    (:SKIP . ,(length bloc-sinon))
		    ,@bloc-sinon)))
	   (:PROGN (map-li2vm (cdr expr) nbparam))
	   (:LOOP (progn
		    (setf loop-stop-cond (li2vm (second expr) nbparam))
		    (setf loop-body (li2vm (third expr) nbparam))
		    `(,@loop-stop-cond
		      (:SKIPNIL . ,(1+ (length loop-body)))
		      ,@loop-body
		      (:SKIP . ,(- 0 2 (length loop-body) (length loop-stop-cond))))))
	   (t (error "~s not a li expression" (car expr))))))

(defun fun2vm (fun)
  (let ((fun-value (get-defun fun)))
    (if (null fun-value)
	(error "~s n'a pas de valeur fonctionnelle" fun)
    ;; on definit une etiquette pour la fonction
      `((:LABEL ,fun)
	;; nombre de vars locales - nbargs - 1
	(:STACK . ,(- (third fun-value) (second fun-value) 1))
	,@(li2vm (fourth fun-value) (second fun-value))
	(:RTN)))))

;; prend une liste d'expressions LI en param
;; et retourne cette meme liste traduite en expressions
;; du langage vm
(defun map-li2vm (lexpr nbparam)
  (if (atom lexpr)
      lexpr
    `(,@(li2vm (first lexpr) nbparam) ,@(map-li2vm (rest lexpr) nbparam))))

(defun gen-setvars (nbargs)
  (let ((lsetvars nil)
	(i 0))
    (loop while (< i nbargs) do
	  (progn
	    (setf lsetvars `(,@lsetvars (:SET-VAR . ,(- nbargs i))))
	    (setf i (1+ i))))
    lsetvars))
