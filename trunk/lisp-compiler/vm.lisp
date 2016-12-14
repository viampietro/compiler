;; associe au symbole name 3 tableaux (code, data-stack, control-stack)
;; et 3 pseudos registres CO, SP et FP.
(defun make-vm (vm-name code-size data-stack-size control-stack-size)
  (progn
    (setf (get vm-name :code) (make-array code-size))
    (setf (get vm-name :data-stack) (make-array data-stack-size))
    (setf (get vm-name :control-stack) (make-array control-stack-size))
    (setf (get vm-name :CO) 0) ;; compteur ordinal du tableau de code
    (setf (get vm-name :DSP) 0) ;; pour data stack pointer
    (setf (get vm-name :FP) 0)  ;; frame pointer de la data-stack
    (setf (get vm-name :CSP) 0) ;; pour control stack pointer
    (setf (get vm-name :htfr) (make-hash-table :test 'equal)) ;; hash table for forward references
    (setf (get vm-name :htss) (make-hash-table :test 'equal)) ;; hash table for solved symbols
    vm-name))

(defun eval-vm (vm expr)
  (if (equal 'defun (car expr))
      (error "eval-vm ne peut pas s'appliquer sur defun")
    (let ((start-co (get vm :CO)))
      (load-vm vm `(,@(li2vm (lisp2li expr '()) (length (cdr expr))) (:HALT)))
      (exec-vm vm start-co))))

;; prend une vm et la valeur du compteur ordinal
;; puis lance l'execution du code de la vm a partir de co.
(defun exec-vm (vm co)
  (let ((not-finished t))
    (loop while not-finished do
	  (let ((instr (read-code vm co)))
	    (format t "~s~%" instr)
	    (ecase (car instr)
		   ;; empile la valeur et incremente le data stack pointer
		   (:CONST (progn
			     ;; on incremente le pointeur de sommet de pile
			     ;; avant d'ajouter une valeur 
			     (write-data-stack vm (get vm :DSP) (cdr instr))
			     (incf (get vm :DSP))
			     (incf co)))

		   ;; empile la valeur de la variable lue a l'adresse frame pointer + (numvar - 1)
		   (:VAR (progn
			   
			   (write-data-stack vm (get vm :DSP) (read-data-stack vm (+ (get vm :FP) (- (cdr instr) 1))))
			   (incf (get vm :DSP))
			   (incf co)))
		   
		   ;; charge la valeur du sommet de pile dans la variable d'adresse frame pointer + (numvar - 1)
		   ;; et depile
		   (:SET-VAR (progn
			       (write-data-stack vm (+ (get vm :FP) (cdr instr)) (read-data-stack vm (get vm :DSP)))
			       (decf (get vm :DSP))
			       (incf co)))

		   (:SKIP (setf co (+ (cdr instr) co)))

		   ;; si le sommet de la pile est egal a nil on saute le nb d'instructions indique par le cdr
		   ;; sinon on incremente le co de 1.
		   ;; On depile quelque soit la valeur du sommet de pile
		   (:SKIPNIL (progn
			       (if (null (read-data-stack vm (get vm :DSP)))
				   (setf co (+ (cdr instr) co))
				 (incf co))
			       (decf (get vm :DSP))))
		   
		   ;; on depile le nb d'arguments dans le let
		   (:CALL (let ((nbargs (read-data-stack vm (decf (get vm :DSP)))) (args-list nil))
			    ;; si ce qui suit :CALL n'est pas une adresse
			    (if (not (integerp (cadr instr)))
				;; si le label n'est pas connu par lisp
				(if (not (fboundp (cadr instr)))
				    (error "~s is undefined" (cadr instr))
				  (progn
				    (loop for i from 0 to (1- nbargs)
					  do (progn
					       (decf (get vm :DSP))
					       (setf args-list `(,(read-data-stack vm (get vm :DSP)) ,@args-list))))
				    (format t "function : ~s, args : ~s~%" (cadr instr) args-list)
				    (write-data-stack vm (get vm :DSP) (apply (cadr instr) args-list))))
			      ;; sinon :CALL est suivi par une adresse (fonction chargee dans la vm)
			      (progn
				;; empiler l'adresse de retour dans la control stack puis empiler la valeur de :FP
				(write-control-stack vm (get vm :CSP) (+ co 1))
				(incf (get vm :CSP)) ;; CSP = CSP + 1
				(write-control-stack vm (get vm :CSP) (get vm :FP))
				(incf (get vm :CSP))
				
				;; calculer la valeur du nveau :FP
				(set-register vm :FP (- (get vm :DSP) nbargs))

				;; saut a l'adresse de la fonction pointee par CALL
				(setf co (cadr instr))))))

		   ;; on reserve l'espace memoire pour les vars locales en augmentant
		   ;; le pointeur de pile de la valeur qui suit :STACK
		   (:STACK (progn
			     (set-register vm :DSP (+ (get vm :DSP) (cdr instr)))
			     (incf co)))
		   
		   (:RTN (let ((return-value (read-data-stack vm (get :DSP)))) ;; on stocke la valeur de retour dns la var return-value

			   ;; on place la valeur de du frame pointer dans data-stack pointer
			   ;; (operation de dechargement du contexte d'appel d'une fonction lors du retour) 
			   (set-register vm :DSP (get vm :FP))

			   ;; on remet dans le frame pointer son ancienne valeur qui etait stockee
			   ;; en haut de la pile de controle (retour au contexte de la fonction appelante)
			   (set-register vm :FP (read-control-stack vm (get :CSP)))
			   (decf (get vm :CSP)) ;; decremente le control stack pointer

			   ;; saute a l'adresse de retour stockee au sommet de la control stack
			   (setf co (get vm :CSP))
			   (decf (get vm :CSP)))) ;; decremente le control stack pointer
		   
		   (:HALT (setf not-finished nil)))))))

;; prend une vm et des instructions en langage vm en arguments
;; parcourt chaque instr du code et les charge dans la pile de code
;; de la vm (avec transformations sur le code selon les instrs lues)
(defun load-vm (vm code)
  (let ((co (get vm :CO)))
    (progn
      (loop for instr in code do
	    (ecase (car instr)
		   ((:CONST :VAR :SET-VAR :SKIP :SKIPNIL :STACK :RTN :HALT)
		    (progn
		      (write-code vm co instr)
		      (setf co (+ co 1))))
		   (:LABEL (load-label (cadr instr) co vm))
		   (:CALL (progn
			    (if (not (fboundp (cadr instr)))
				(load-call instr co vm))
			    (write-code vm co instr)
			    (setf co (+ co 1))))))
      ;; conserve la postion du compteur ordinal a la fin du chargement
      ;; pour pouvoir loader du code en plusieurs appels
      (setf (get vm :CO) co))))

;; prend en arguments une instruction (instr) de type (:CALL label),
;; l'adresse (addr) de cette instruction et la vm associee (vm).
;; Remplace le label par l'adresse du label (dans l'instruction :CALL) si celui-ci est un symbole connu,
;; sinon remplit la hash-table des refs en avant en associant le label
;; avec l'adresse de l'instruction :CALL qu'il faudra resoudre plus tard.
(defun load-call (instr addr vm)
  (let ((label-addr (gethash (cadr instr) (get vm :htss)))
	(label-fr (gethash (cadr instr) (get vm :htfr))))
    ;; si le label appartient a la hash-table des solved symbols
    (if (not (null label-addr))
	(setf (cadr instr) label-addr) ;; on remplace le label par son adresse dans l'instruction
      ;; si le label est deja associe a des refs en avant
      (if (not (null label-fr))
	  ;; on ajoute une nouvelle addr a la liste des refs en avant
	  ;; (adresse ou se trouve le call en cours de traitement)
	  (setf (gethash (cadr instr) (get vm :htfr)) `(,@label-fr ,addr))
	(setf (gethash (cadr instr) (get vm :htfr)) `(,addr))))))

;; Prend en arguments un label de fonction, l'adresse du label
;; dans le code de la vm, et la vm associee.
;; Associe le label a l'adresse addr dans la hast-table des solved symbols
;; et appelle la fonction resolve-fr pour modifier les references en avant
;; sur ce label (si il y en a).
;; Genere une erreur si le label est deja utilise.
(defun load-label (label addr vm)
  (if (gethash label (get vm :htss))
      (error "~s already define" label)
    (progn
      (setf (gethash label (get vm :htss)) addr) ;; assoc label-addr
      (if (gethash label (get vm :htfr)) ;; si label a ete reference en avant
	  (resolve-fr (gethash label (get vm :htfr)) addr vm)))))

;; parcours la liste des references en avant associees a un label (qui est maintenant connu)
;; et change toutes les instructions de la forme (:CALL label) par
;; (:CALL label-addr).
;; Et associe au symbole label dans htfr une liste d'adresses ou se situent
;; les appels (:CALL label)
(defun resolve-fr (lfr label-addr vm)     
  (if (atom lfr)
      ()
    (progn
      (write-code vm (car lfr) `(:CALL ,label-addr))
      (resolve-fr (cdr lfr) label-addr vm))))

;; renvoie le tableau correspondant au code charge
;; dans la vm
(defun get-code (vm)
  (get vm :code))

;; renvoie l'instruction contenu dans la pile de code
;; a l'adresse addr
;; ne peut pas etre utilisee dans write-code car pas compatible
;; avec setf
(defun read-code (vm addr)
  (aref (get-code vm) addr))

;; ecriture de value dans le tableau de code de la vm
;; a l'adresse pointee par addr
(defun write-code (vm addr value)
  (setf (aref (get-code vm) addr) value))

;; renvoie le tableau correspondant a la pile 
;; de donnees de la vm
(defun get-data-stack (vm)
  (get vm :data-stack))

;; renvoie le contenu de la pile de donnees
;; a l'adresse addr
(defun read-data-stack (vm addr)
  (aref (get-data-stack vm) addr))

;; ecriture de value a l'adresse addr dans
;; la pile de donnees
(defun write-data-stack (vm addr value)
  (setf (aref (get-data-stack vm) addr) value))

;; renvoie le tableau correspondant a la pile 
;; de controle de la vm
(defun get-control-stack (vm)
  (get vm :control-stack))

;; renvoie le contenu de la pile de controle
;; a l'adresse addr
(defun read-control-stack (vm addr)
  (aref (get-control-stack vm) addr))

;; ecriture de value a l'adresse addr dans
;; la pile de controle
(defun write-control-stack (vm addr value)
  (setf (aref (get-control-stack vm) addr) value))

;; ecriture de value dans le pseudo-registre de la vm
;; pointe par reg
(defun set-register (vm reg value)
  (setf (get vm reg) value))


