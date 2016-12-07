;; associe au symbole name 3 tableaux (code, data-stack, control-stack)
;; et 3 pseudos registres CO, SP et FP.
(defun make-vm (name code-size data-stack-size control-stack-size)
  (progn
    (setf (name :code) (make-array code-size))
    (setf (name :data-stack) (make-array data-stack-size))
    (setf (name :control-stack) (make-array control-stack-size))
    (setf (name :CO) 0)
    (setf (name :SP) 0)
    (setf (name :FP) 0)
    (setf (name :htfr) (make-hash-table :test equal))
    (setf (name :htss) (make-hash-table :test equal))
    name))

;; prend une vm et la valeur du compteur ordinal
;; puis lance l'execution du code de la vm a partir de co.
(defun exec-vm (vm co)
  (let ((not-finished t))
    (loop while not-finished do
	  (let ((instr (read-code vm co)))
	    (ecase (car instr)
		   (:CONST (write-data vm (get-register vm :SP) (cadr instr)))
		   (:HALT (setf not-finished nil)))))))

(defun load-vm (vm code)
  (let ((co (get-register vm :CO)))
    (loop for instr in code do
	  (ecase (car instr)
		 ((:CONST :VAR :SET-VAR :SKIP :SKIPNIL :STACK)
		  (progn
		    (write-code vm co instr)
		    (setf co (+ co 1))))
		 (:LABEL (load-label (cadr instr) co vm))))


;; renvoie le tableau correspondant au code charge
;; dans la vm
(defun get-code (vm)
  (get vm :code))

;; renvoie l'instruction contenu dans la pile de code
;; a l'adresse addr
(defun read-code (vm addr)
  (aref (get-code vm) addr))

;; ecriture de value dans le tableau de code de la vm
;; a l'adresse pointee par addr
(defun write-code (vm addr value)
  (setf (read-code vm addr) value))

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
  (setf (read-data-stack vm addr) value))

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
  (setf (read-control-stack vm addr) value))


;; retourne la valeur du pseudo-registre reg
;; de la vm
(defun get-register (vm reg)
  (get (vm reg)))

;; ecriture de value dans le pseudo-registre de la vm
;; pointe par reg
(defun set-register (vm reg value)
  (setf (vm reg) value))


