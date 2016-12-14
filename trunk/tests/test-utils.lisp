(require "../lisp-interpret/lisp2li.lisp")
(require "../lisp-interpret/eval-li.lisp")
(require "../lisp-compiler/vm.lisp")
(require "../lisp-compiler/li2vm.lisp")

;; prend un nom de fichier en parametre, lit son contenu
;;(plus precisement le premier objet lisp qui est contenu dans le fichier)
;; et le retourne puis ferme le flux de lecture.
(defun load-fun (fun-name)
  (let ((stream (open fun-name)))
    (prog1
	(read stream)
      (close stream))))

;; affiche les instructions presentes dans la pile de code
;; de la vm
(defun display-code (vm)
  (loop for i from 0 to ( - (get-register vm :CO) 1)
	do (format t "~s~%" (read-code vm i))))

(defun display-data-stack (vm)
  (progn
    (loop for i from 0 to (- (get-register vm :DSP) 1) 
	  do (format t "~s " (read-data-stack vm i)))
    (format t "~%")))

(defun meval (expr)
  (eval-li (lisp2li expr '()) '#(nil)))

