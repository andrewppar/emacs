  (defun end-of-buffer-p ()
    (let ((current (point))
	  (end     nil))
      (save-excursion
	(end-of-buffer)
	(setq end (point)))
      (>= current end)))

  ;; For wrapping tables in src blocks
  (defmacro org--table-enter-or-exit (exit?)
    `(let ((at-table? ,exit?))
       (while ,(if exit?
		   'at-table?
		 '(not at-table?))
	 (forward-line)
	 (when ,(if exit?
		    '(not (org-at-table-p))
		  '(or (org-at-table-p)
		       (end-of-buffer-p)))
	   (setq at-table? ,(not exit?))))))

  (defun org-to-next-table ()
    (interactive)
    (org--table-enter-or-exit nil))

  (defun org-exit-current-table ()
    (interactive)
    (org--table-enter-or-exit t))

  (defun org-src-tables ()
    (interactive)
    (while (not (end-of-buffer-p))
      (org-to-next-table)
      (let ((begin (point)))
	(org-exit-current-table)
	(let ((end (point)))
	  (goto-char begin)
	  (insert "#+BEGIN_SRC\n")
	  (goto-char end)
	  (forward-line)
	  (insert "#+END_SRC\n")))
      (forward-line)))
