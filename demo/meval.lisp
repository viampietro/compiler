;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    MEVAL SIMPLE de fonctions recursives (fibo et fact) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; en premier se mettre dans le dossier tests et lancer clisp
;; puis charger les fichiers lisp necessaires avec : 
(load "test-utils.lisp")

;; pour evaluer (fibo 10) par exemple
;; on meta-evalue (defun fibo (n) (...)) d'abord

;; meval == eval-li(lisp2li ())
(meval (load-fun 'fibo)) ;; l'appel recursif a fibo dans fibo est considere comme :UNKNOWN pour l'instant

;; maintenant que fibo a ete evaluee, on peut l'appeler
;; et au premier appel les :UNKNOWN seront transformes en :MCALL
;; puisque fibo est maintenant connue
(meval '(fibo 10))

;; pour pouvoir evaluer et lancer fibo avec l'interprete lisp natif
(eval (load-fun 'fibo))
(eval '(fibo 10))

;; on peut utiliser la fonction time pour comparer le temps d'execution des deux
(time (meval '(fibo 10)))
(time (eval '(fibo 10)))
;; => raisonnable entre 10^3, 10^4 secs de difference

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          MEVAL du meta-evaluateur           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pour meta-evaluer le meta-evaluateur, il faut que lisp2li, eval-li et meval
;; soient definis en tant que symbole fonctionnel utilisateur.
;; C'est a dire, on va associer a un symbole lisp2li (respectivement eval-li, meval)
;; un attribut :defun dont la valeur sera de la forme (:LAMBDA <nbargs> <taille env> <corps>)

(meval (load-fun 'lisp2li)) 
(meval (load-fun 'eval-li))
(meval (load-fun 'meval))

;; OU

(meval-interpret)

;; a la premiere meta-evaluation de lisp2li (respectivement eval-li, meval)
;; le appel recursif a lisp2li dans lisp2li seront traduits par des :CALL
;; car le fonction lisp2li a bien ete definie dans l'interprete lisp natif

;; pour transformer les :CALL en :MCALL, il faut en fait relancer une evaluation de lisp2li
;; (un deuxieme (meval (load-fun 'lisp2li))) qui prendra en compte la premiere meta-definition
;; de lisp2li lors du parcours du (defun lisp2li () ...)

(meval (load-fun 'lisp2li)) 
(meval (load-fun 'eval-li))
(meval (load-fun 'meval))
