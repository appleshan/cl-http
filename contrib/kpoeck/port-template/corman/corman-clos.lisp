(in-package :cl)

#+no
(defun kludge-arglist (lambda-list)
  (if (and (member '&key lambda-list)
           (not (member '&allow-other-keys lambda-list)))
      (extend-lambda-list lambda-list '(&allow-other-keys))
      (if (and (not (member '&rest lambda-list))
               (not (member '&key lambda-list)))
          (extend-lambda-list lambda-list '(&key &allow-other-keys))
        lambda-list)))

#+no
(defun extend-lambda-list (list extension)
  (if (position '&aux list)
      (append (subseq list 0 (position '&aux list)) extension (subseq list (position '&aux list)))
    (append list extension)))

(defclass logical-pathname ()())

#|

(kludge-arglist '(a b c &key b &aux hugo otto))
(kludge-arglist '(a b c &key b ))
|#