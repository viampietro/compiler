(require "../lisp2li.lisp")
(require "../eval-li.lisp")

;; prend un nom de fichier en parametre, lit son contenu
;;(plus precisement le premier objet lisp qui est contenu dans le fichier)
;; et le retourne puis ferme le flux de lecture.
(defun load-fun (fun-name)
  (let ((stream (open fun-name)))
    (prog1
	(read stream)
      (close stream))))

(defun meval (expr)
  (eval-li (lisp2li expr '()) '#(nil)))

