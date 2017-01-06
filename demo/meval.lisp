;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    MEVAL SIMPLE


;; en premier se mettre dans le dossier tests et lancer clisp
;; puis charger les fichiers lisp necessaires a la compil et interpretation
(load "test-utils.lisp")

;; pour evaluer (fibo 10) par exemple
;; on meta-evalue (defun fibo (n) (...)) d'abord

;; meval == eval-li(lisp2li ())
(meval (load-fun 'fibo)) ;; l'appel recursif a fibo dans fibo est considere comme :UNKNOWN pour l'instant

;; maintenant que fibo a ete evaluee, on peut l'appeler
;; et au premier appel les UNKNOWN seront transformes en :MCALL
(meval '(fibo 10))

;; pour pouvoir evaluer et lancer fibo avec l'interprete lisp natif
(eval (load-fun 'fibo))
(eval '(fibo 10))

;; on peut utiliser la fonction time pour comparer le temps d'execution des deux
(time (meval '(fibo 10)))
(time (eval '(fibo 10)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          MEVAL du meta-evaluateur

;; pour meta-evaluer le meta-evaluateur, il faut que lisp2li, eval-li et meval
;; soient definis en tant que symbole fonctionnel utilisateur.
;; C'est a dire la fonction (get-defun 'symb) renvoie une valeur fonctionnelle de type:
;; (:LAMBDA <nbargs> <taille env> <corps>)
(meval (load-fun 'lisp2li))
(meval (load-fun 'eval-li))
(meval (load-fun 'meval))

;; OU

(meval-interpret)
