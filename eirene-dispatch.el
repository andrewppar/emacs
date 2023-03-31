(defmacro defgeneric
    (name arglist &rest body)
  (let ((gen-fn  (intern (format "*generic-%s*" name))))
    `(progn
       (defvar ,gen-fn '())
       ,(when body
	  `(setq ,gen-fn
		 (cons
		  (cons :default (lambda ,arglist ,@body))
		  ,gen-fn))))))

(defun execute-method (method-name keyword args)
  (let ((method-list (intern (format "*generic-%s*" method-name)))
