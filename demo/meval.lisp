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

;; pour meta-evaluer le meta-evaluateur, il faut que lisp2li, eval-li et meval (et toutes les fonctions annexes)
;; soient definis en tant que symbole fonctionnel utilisateur.
;; C'est a dire, on va associer a un symbole lisp2li (respectivement eval-li, meval, et annexes)
;; un attribut :defun dont la valeur sera de la forme (:LAMBDA <nbargs> <taille env> <corps>)

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

;; OU

(meval-interpret) ;; meval-interpret fait un double appel de toutes les fonctions

;; a la premiere meta-evaluation de lisp2li (respectivement eval-li, meval)
;; l'appel recursif a lisp2li dans lisp2li seront traduits par des :CALL
;; car le fonction lisp2li a bien ete definie dans l'interprete lisp natif

;; pour transformer les :CALL en :MCALL, il faut en fait relancer une evaluation de lisp2li
;; (un deuxieme (meval (load-fun 'lisp2li))) qui prendra en compte la premiere meta-definition
;; de lisp2li lors du parcours du (defun lisp2li () ...)
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


;; on peut par la suite pratiquer des appels du type :

(meval '(meval '(fact 5)))
(meval '(meval '(fibo 20)))

;; Attention, on peut faire appel à meval seulement
;; a deux niveau d'imbrication.
;; Les lignes suivantes generent une erreur :
(meval '(meval '(meval '(fibo 20))))
(meval '(meval '(lisp2li '(fibo 20) '())))


;; on peut méta-evaluer le compilateur avec la fonction (meval-compiler)
;; On appelle compilateur toutes les fonctions de traduction
;; du langage lisp, li en assembleur (langage vm)
(meval-compiler)

;; on va alors meta-evaluer li2vm, fun2vm, ...
;; on peut maintenant appeler ces fonctions de compilation via le meta-evaluateur
(meval '(mcompile '(fact 5)))
(meval '(mcompile (load-fun 'fact)))


;; On peut méta-evaluer la vm avec la fonction :
(meval-vm) ;; meta-eval de load-vm, exec-vm, eval-vm, ...

;; puis on peut creer et lancer une vm depuis le meta-evaluateur :
(meval '(make-vm 'ma-vm 1000 1000 1000)) ;; cree la vm depuis le meta-evaluateur
(meval '(load-vm 'ma-vm (fun2vm 'fact))) ;; charge fact dans la vm depuis le meta-evaluateur

(meval '(eval-vm 'ma-vm '(fact 5))) ;; evalue (fact 5) dans la vm, evaluation lancee depuis le meta-evaluateur
;; ATTENTION aux appels gourmants, (meval (eval-vm 'ma-vm '(fibo 11))) fait devorder la pile lisp

