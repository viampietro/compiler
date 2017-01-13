;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      VM : CREATION, CHARGEMENT ET EXECUTION        ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pour utiliser la vm, il faut d'abord la creer
(make-vm 'ma-vm 100 100 100)

;; puis pour pouvoir charger une fonction dans la vm
;; il faut d'abord la meta-evaluee avec meval
(meval (load-fun 'fibo))

;; puis il faut traduire fibo en langage vm, puis charger le code vm
;; dans le tableau de code de la vm
(load-vm 'ma-vm (fun2vm 'fibo)) ;; fun2vm prend la valeur fonctionnelle en li
					; du symbole fibo et la traduit en lgage vm

;; enfin, on peut executer fibo dans la vm en faisant appel a la
;; fonction eval-vm
(eval-vm 'ma-vm '(fibo 10))

;; On peut charger le meta-evaluateur dans la vm, et
;; lancer son execution dans la vm :
(load-vm 'ma-vm (fun2vm 'lisp2li))
(load-vm 'ma-vm (fun2vm 'eval-li))
(load-vm 'ma-vm (fun2vm 'meval))

;; puis l'execution
(eval-vm 'ma-vm '(meval '(fact 5)))

;; On peut enfin charger load-vm dans la vm et utiliser la version de load-vm
;; qui se trouve dans la vm pour charger d'autres fonctions

;; D'abord on fait load-vm sur defun load-vm :
(load-vm 'ma-vm (fun2vm 'load-vm))

;; puis on charge fibo depuis la fonction load-vm qui
;; est deja chargee dans la vm (presente dans le tableau de code)
(eval-vm 'ma-vm '(load-vm 'ma-vm (fun2vm 'fibo)))






