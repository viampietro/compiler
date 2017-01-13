(require "../lisp-interpret/lisp2li.lisp")
(require "../lisp-interpret/eval-li.lisp")
(require "../lisp-compiler/vm.lisp")
(require "../lisp-compiler/li2vm.lisp")

;; prend un nom de fichier en parametre, lit son contenu
;;(plus precisement le premier objet lisp qui est contenu dans le fichier)
;; et le retourne puis ferme le flux de lecture.
(defun load-fun (fun-name)
  (let ((stream (open fun-name)))
    (prog1
	(read stream)
      (close stream))))

;; affiche les instructions presentes dans la pile de code
;; de la vm
(defun display-code (vm)
  (loop for i from 0 to ( - (get vm :CO) 1)
	do (format t "~s~%" (read-code vm i))))

(defun display-data-stack (vm)
  (progn
    (loop for i from 0 to (- (get vm :DSP) 1) 
	  do (format t "~s " (read-data-stack vm i)))
    (format t "~%")))

(defun display-control-stack (vm)
  (progn
    (loop for i from 0 to (- (get vm :CSP) 1) 
	  do (format t "~s " (read-control-stack vm i)))
    (format t "~%")))

;; Retourne le nom de la fonction nouvellement definie
;; si expr est un defun. Permet d'enchainer l'appel avec fun2vm.
(defun meval (expr)
  (if (eql (car expr) 'defun)
      (progn
	(eval-li (lisp2li expr '()) '#(nil))
	(second expr))
    (eval-li (lisp2li expr '()) '#(nil))))

(defun meval-interpret ()
  (progn
    ;; premiere passe genere des :CALL
    (meval (load-fun 'let2setf))
    (meval (load-fun 'cond2if))
    (meval (load-fun 'case2if))
    (meval (load-fun 'map-lisp2li))
    (meval (load-fun 'lisp2li))
    (meval (load-fun 'map-eval-li))
    (meval (load-fun 'map-eval-li-progn))
    (meval (load-fun 'make-eval-li-env))
    (meval (load-fun 'eval-li))
    (meval (load-fun 'meval))

    ;; deuxieme passe genere des :MCALL
    (meval (load-fun 'let2setf))
    (meval (load-fun 'cond2if))
    (meval (load-fun 'case2if))
    (meval (load-fun 'map-lisp2li))
    (meval (load-fun 'lisp2li))
    (meval (load-fun 'map-eval-li))
    (meval (load-fun 'map-eval-li-progn))
    (meval (load-fun 'make-eval-li-env))
    (meval (load-fun 'eval-li))
    (meval (load-fun 'meval))))

;; Composition de lisp2li et li2vm
;; ou appel a fun2vm si expr est un defun.
(defun mcompile (expr)
  (if (eql (car expr) 'defun)
      (fun2vm (meval expr)) 
    (li2vm (lisp2li expr '()) 0)))

(defun meval-compiler ()
  (progn
    (meval (load-fun 'li2vm))
    (meval (load-fun 'fun2vm))
    (meval (load-fun 'map-li2vm))
    (meval (load-fun 'mcompile))

    ;; deuxieme passe pour les :MCALL
    (meval (load-fun 'li2vm))
    (meval (load-fun 'fun2vm))
    (meval (load-fun 'map-li2vm))
    (meval (load-fun 'mcompile))))

(defun meval-vm ()
  (progn
    (meval (load-fun 'make-vm))
    (meval (load-fun 'reset-vm))
    (meval (load-fun 'load-vm))
    (meval (load-fun 'exec-vm))
    (meval (load-fun 'eval-vm))

    ;; deuxieme passe pour les :MCALL
    (meval (load-fun 'make-vm))
    (meval (load-fun 'reset-vm))
    (meval (load-fun 'load-vm))
    (meval (load-fun 'exec-vm))
    (meval (load-fun 'eval-vm))))

    
(defun marque-terminal-li (fun)
  (if (get-defun fun)
      (map-marque-terminal-li (fourth (get-defun fun)))
    (error "le symbole ~s n'existe pas" fun)))

(defun map-marque-terminal-li (expr-li)
  (case (car expr-li)
	(:MCALL (setf (car expr-li) :MCALLT))
	(:CALL (setf (car expr-li) :CALLT))
	(:IF (progn
	       (map-marque-terminal-li (third expr-li))
	       (map-marque-terminal-li (fourth expr-li))))
	(:PROGN (map-marque-terminal-li (nth (- (length (cdr expr-li)) 1) (cdr expr-li))))
	(t nil)))

