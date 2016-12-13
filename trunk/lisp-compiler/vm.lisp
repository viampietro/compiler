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

;; prend une vm et la valeur du compteur ordinal
;; puis lance l'execution du code de la vm a partir de co.
(defun exec-vm (vm co)
  (let ((not-finished t))
    (loop while not-finished do
	  (let ((instr (read-code vm co)))
	    (ecase (car instr)
		   ;; empile la valeur et incremente le data stack pointer
		   (:CONST (progn
			     (write-data-stack vm (get-register vm :DSP) (cdr instr))
			     (set-register vm :DSP (+ 1 (get-register vm :DSP)))
			     (setf co (+ 1 co))))

		   ;; empile la valeur de la variable lue a l'adresse frame pointer + (numvar - 1)
		   (:VAR (progn
			   (write-data-stack vm (get-register vm :DSP) (read-data-stack vm (+ (get-register vm :FP) (- (cdr instr) 1))))
			   (set-register vm :DSP (+ 1 (get-register vm :DSP)))
			   (setf co (+ 1 co))))
		   
		   ;; charge la valeur du sommet depile dans la variable d'adresse frame pointer + (numvar - 1)
		   ;; et depile
		   (:SET-VAR (progn
			       (write-data-stack vm (+ (get-register vm :FP) (- (cdr instr) 1)) (read-data-stack vm (get-register vm :DSP)))
			       (set-register vm :DSP (- (get-register vm :DSP) 1))
			       (setf co (+ 1 co))))

		   (:SKIP (setf co (+ (cdr instr) co)))

		   ;; si le sommet de la pile est egal a nil on saute le nb d'instructions indique par le cdr
		   ;; sinon on incremente le co de 1.
		   ;; On depile quelque soit la valeur du sommet de pile
		   (:SKIPNIL (progn
			       (if (null (read-data-stack vm (get-register vm :DSP)))
				   (setf co (+ (cdr instr) co))
				 (setf co (+ 1 co)))
			       (set-register vm :DSP (- (get-register vm :DSP) 1))))

		   (:CALL (let ((nbargs (read-data-stack vm (get-register vm :DSP))))
			    (progn
			      ;; empiler l'adresse de retour dans la control stack puis la valeur de :FP
			      (write-control-stack vm (get-register vm :CSP) (+ co 1))
			      (set-register vm :CSP (+ (get-register vm :CSP) 1)) ;; CSP = CSP + 1
			      (write-control-stack vm (get-register vm :CSP) (get-register vm :FP))
			      (set-register vm :CSP (+ (get-register vm :CSP) 1)) ;; CSP = CPS + 1
			      
			      ;; depiler le nb d'args et calculer la valeur du nveau :FP
			      (set-register vm :DSP (- (get-register vm :DSP) 1))
			      (set-register vm :FP (- (get-register vm :DSP) nbargs))

			      ;; saut a l'adresse de la fonction pointee par CALL
			      (setf co (cdr instr)))))

		   (:STACK ())
		   
		   (:HALT (setf not-finished nil)))))))

;; prend une vm et des instructions en langage vm en arguments
;; parcourt chaque instr du code et les charge dans la pile de code
;; de la vm (avec transformations sur le code selon les instrs lues)
(defun load-vm (vm code)
  (let ((co (get-register vm :CO)))
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

;; retourne la valeur du pseudo-registre reg
;; de la vm
(defun get-register (vm reg)
  (get vm reg))

;; ecriture de value dans le pseudo-registre de la vm
;; pointe par reg
(defun set-register (vm reg value)
  (setf (get vm reg) value))


