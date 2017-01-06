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

