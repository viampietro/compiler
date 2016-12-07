(require "../lisp-interpret/lisp2li.lisp")
(require "../lisp-interpret/eval-li.lisp")
(require "./li2vm.lisp")

(defun check-li2vm (expr-li expr-vm nbparam)
  (let ((expr-vm-res (li2vm expr-li nbparam)))
    (if (equal expr-vm expr-vm-res)
	(format t "~s => ~s~%" expr-li expr-vm-res)
      (warn "For ~s expected ~s received ~s" expr-li expr-vm expr-vm-res))))

(defun test-li2vm ()
  (progn
    (check-li2vm (lisp2li 1 '()) '((:CONST 1)) 0)
    (check-li2vm (lisp2li 'n '(n)) '((:VAR 1)) 1)
    (check-li2vm (lisp2li '(setf n 2) '(n)) '((:CONST 2)(:SET-VAR 1)) 1)
    (check-li2vm (lisp2li '(+ 1 2) '()) '((:CONST 1) (:CONST 2) (:CALL +)) 0)
    (check-li2vm (lisp2li '(if (< 1 2) (+ 2 4) 3) '())
		 '((:CONST 1)
		   (:CONST 2)
		   (:CALL <)
		   (:SKIPNIL 3)
		   (:CONST 2)
		   (:CONST 4)
		   (:CALL +)
		   (:CONST 3)) 0)
    ))

;; prend une expression lisp correspondant a un defun (+ symbole correspondant)
;; et verirfie que la traduction en langage vm est correcte
(defun check-fun2vm (expr fun expr-vm nbparam)
  (progn
    (eval-li (lisp2li expr '()) '#(nil))
    (let ((expr-vm-res (fun2vm fun)))
      (if (equal expr-vm expr-vm-res)
	  (format t "~s => ~s~%" expr expr-vm-res)
	(warn "For ~s expected ~s received ~s" expr expr-vm expr-vm-res)))))

(defun test-fun2vm ()
  (progn
    (check-fun2vm '(defun square (n) (* n n)) 'square
		  '((:LABEL square)
		    (:STACK 0)
		    (:VAR 1)
		    (:VAR 1)
		    (:CONST 2)
		    (:CALL *)
		    (:RTN)) 0)))

