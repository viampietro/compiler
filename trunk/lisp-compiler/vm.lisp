;; associe au symbole name 3 tableaux (code, data-stack, control-stack)
;; et 3 pseudos registres CO, SP et FP.
(defun make-vm (vm-name code-size data-stack-size control-stack-size)
  (progn
    (set-register vm-name :code (make-array code-size))
    (set-register vm-name :data-stack (make-array data-stack-size))
    (set-register vm-name :control-stack (make-array control-stack-size))
    (set-register vm-name :CO 0) ;; compteur ordinal du tableau de code
    (set-register vm-name :EOC 0) ;; end of code, pointeur sur la fin du tableau de code
    (set-register vm-name :DSP 0) ;; pour data stack pointer
    (set-register vm-name :FP 0)  ;; frame pointer de la data-stack
    (set-register vm-name :CSP 0) ;; pour control stack pointer
    (set-register vm-name :htfr (make-hash-table :test 'equal)) ;; hash table for forward references
    (set-register vm-name :htss (make-hash-table :test 'equal)) ;; hash table for solved symbols
    vm-name))

(defun reset-vm (vm-name)
  (progn
    (set-register vm-name :code (make-array (array-total-size (get vm-name :code))))
    (set-register vm-name :data-stack (make-array (array-total-size (get vm-name :data-stack))))
    (set-register vm-name :control-stack (make-array (array-total-size (get vm-name :control-stack))))
    (set-register vm-name :CO 0) ;; compteur ordinal du tableau de code
    (set-register vm-name :DSP 0) ;; pour data stack pointer
    (set-register vm-name :FP 0)  ;; frame pointer de la data-stack
    (set-register vm-name :CSP 0) ;; pour control stack pointer
    (set-register vm-name :htfr (make-hash-table :test 'equal)) ;; hash table for forward references
    (set-register vm-name :htss (make-hash-table :test 'equal)) ;; hash table for solved symbols
    vm-name))

(defun eval-vm (vm expr)
  (let ((start-co (get vm :CO)))
    (if (equal 'defun (car expr))
	(error "eval-vm ne peut pas s'appliquer sur defun")
      (progn
	(if (fboundp 'lisp2li)
	    (load-vm vm `(,@(li2vm (lisp2li expr '()) (length (cdr expr))) (:HALT)))
	  (load-vm vm `(,@(li2vm (meval `(lisp2li ,expr '())) (length (cdr expr))) (:HALT))))
	(exec-vm vm start-co)
	;; lecture du resultat sur le sommet de la data stack
	;; et mise a zero du data stack pointer
	;; (set-register vm :CO start-co)
	(read-data-stack vm (set-register vm :DSP (1- (get vm :DSP))))))))

;; prend une vm et la valeur du compteur ordinal
;; puis lance l'execution du code de la vm a partir de co.
(defun exec-vm (vm co)
  (let ((not-finished t)
	(instr nil)
	(nbargs nil)
	(args-list nil)
	(i nil)
	(return-value nil))
    (loop while not-finished do
	  (progn
	    (setf instr (read-code vm co))
	    ;; (format t "INSTR n° ~s : ~s~%" co instr)
	    (case (car instr)
		  ;; empile la valeur et incremente le data stack pointer
		  (:CONST (progn
			    ;; on incremente le pointeur de sommet de pile
			    ;; avant d'ajouter une valeur 
			    (write-data-stack vm (get vm :DSP) (cdr instr))
			    (set-register vm :DSP (1+ (get vm :DSP)))
			    (incf co)))

		  ;; empile la valeur de la variable lue a l'adresse frame pointer + (numvar - 1)
		  (:VAR (progn
			  (write-data-stack vm (get vm :DSP) (read-data-stack vm (+ (get vm :FP) (- (cdr instr) 1))))
			  (set-register vm :DSP (1+ (get vm :DSP)))
			  (incf co)))
		  
		  ;; charge la valeur du sommet de pile dans la variable d'adresse frame pointer + (numvar - 1)
		  ;; et depile
		  (:SET-VAR (progn
			      (write-data-stack vm (+ (get vm :FP) (- (cdr instr) 1)) (read-data-stack vm (set-register vm :DSP (1- (get vm :DSP)))))
			      (incf co)))

		  (:SKIP (setf co (+ (cdr instr) co 1)))

		  ;; si le sommet de la pile est egal a nil on saute le nb d'instructions indique par le cdr
		  ;; sinon on incremente le co de 1.
		  ;; On depile quelque soit la valeur du sommet de pile
		  (:SKIPNIL (if (null (read-data-stack vm (set-register vm :DSP (1- (get vm :DSP)))))
				(setf co (+ (cdr instr) co 1))
			      (incf co)))
		  
		  ;; on depile le nb d'arguments dans le let
		  (:CALL (progn
			   (setf nbargs (read-data-stack vm (set-register vm :DSP (1- (get vm :DSP)))))
			   (setf args-list nil)
			   
			   ;; si ce qui suit :CALL n'est pas une adresse
			   (if (not (integerp (cadr instr)))
			       ;; si le label n'est pas connu par lisp
			       (if (not (fboundp (cadr instr)))
				   (error "~s is undefined" (cadr instr))
				 (progn
				   (setf i 0)
				   (loop while (<= i (1- nbargs))
					 do (progn
					      (set-register vm :DSP (1- (get vm :DSP)))
					      (setf args-list `(,(read-data-stack vm (get vm :DSP)) ,@args-list))
					      (setf i (1+ i))))
				   ;; (format t "args-list = ~s~%" args-list)
				   (write-data-stack vm (get vm :DSP) (apply (cadr instr) args-list))
				   (set-register vm :DSP (1+ (get vm :DSP)))
				   (incf co)))
			     ;; sinon :CALL est suivi par une adresse (fonction chargee dans la vm)
			     (progn
			       ;; empiler l'adresse de retour dans la control stack puis empiler la valeur de :FP
			       (write-control-stack vm (get vm :CSP) (+ co 1))
			       (set-register vm :CSP (1+ (get vm :CSP))) ;; CSP = CSP + 1
			       (write-control-stack vm (get vm :CSP) (get vm :FP))
			       (set-register vm :CSP (1+ (get vm :CSP))) ;; CSP = CSP + 1
			       
			       ;; calculer la valeur du nveau :FP
			       (set-register vm :FP (- (get vm :DSP) nbargs))

			       ;; saut a l'adresse de la fonction pointee par CALL
			       (setf co (cadr instr))))))

		  ;; on reserve l'espace memoire pour les vars locales en augmentant
		  ;; le pointeur de pile de la valeur qui suit :STACK
		  (:STACK (progn
			    (set-register vm :DSP (+ (get vm :DSP) (cdr instr)))
			    (incf co)))
		  
		  (:RTN (progn
			  
			  ;; on stocke la valeur de retour dns la var return-value
			  (setf return-value (read-data-stack vm (set-register vm :DSP (1- (get vm :DSP))))) 
			  
			  ;; on place la valeur de du frame pointer dans data-stack pointer
			  ;; (operation de dechargement du contexte d'appel d'une fonction lors du retour) 
			  (set-register vm :DSP (get vm :FP))
			  (write-data-stack vm (get vm :DSP) return-value)
			  (set-register vm :DSP (1+ (get vm :DSP)))
			  
			  ;; on remet dans le frame pointer son ancienne valeur qui etait stockee
			  ;; en haut de la pile de controle (retour au contexte de la fonction appelante)
			  (set-register vm :FP (read-control-stack vm (set-register vm :CSP (1- (get vm :CSP)))))
			  (set-register vm :CSP (1- (get vm :CSP))) ;; decremente le control stack pointer
			  
			  ;; saute a l'adresse de retour stockee au sommet de la control stack
			  (setf co (read-control-stack vm (get vm :CSP)))))
		  
		  (:HALT (setf not-finished nil))
		  (t (error "instr must be one of :HALT, :RTN, :STACK, :CALL, :SKIPNIL, :SKIP, :SET-VAR, :VAR, :CONST")))
	    
	    ;; INSTRUCTIONS DE DEBUG
	    ;; (format t "DATA STACK : ")
	    ;; (display-data-stack vm)
	    ;; (format t "CONTROL STACK : ")
	    ;; (display-control-stack vm)
	    ;; (format t "DSP = ~s~%" (get vm :DSP))
	    ;; (format t "CSP = ~s~%" (get vm :CSP))
	    ;; (format t "FP = ~s~%" (get vm :FP))
	    ;; (format t "Enter to continue, Q to quit : ")
	    ;; (if (equal "Q" (read-line))
	    ;; 	(return nil)
	    ;;   (format t "-------------------------------~%"))
	    ))))

;; prend une vm et des instructions en langage vm en arguments
;; parcourt chaque instr du code et les charge dans la pile de code
;; de la vm (avec transformations sur le code selon les instrs lues)
(defun load-vm (vm code)
  (let ((co (get vm :CO))
	(instr nil)
	(cell code))
    (progn
      (loop while (consp cell) do
	    (progn
	      (setf instr (car cell))
	      (setf cell (cdr cell))
	      (case (car instr)
		    ((:CONST :VAR :SET-VAR :SKIP :SKIPNIL :STACK :RTN :HALT)
		     (progn
		       (write-code vm co instr)
		       (setf co (+ co 1))))
		    (:LABEL (load-label (cadr instr) co vm))
		    (:CALL (progn
			     (if (get-defun (cadr instr))
				 (load-call instr co vm))
			     (write-code vm co instr)
			     (setf co (+ co 1))))
		    (t (error "instr must be one of :CONST :VAR :SET-VAR :SKIP :SKIPNIL :STACK :RTN :HALT :CALL :LABEL")))))
      ;; conserve la postion du compteur ordinal a la fin du chargement
      ;; pour pouvoir loader du code en plusieurs appels
      (set-register vm :CO co))))

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


