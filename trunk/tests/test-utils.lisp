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
