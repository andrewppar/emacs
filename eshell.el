(defvar *eshell-prompt-icon* "⚓")

(make-variable-buffer-local
 (defvar *last-command-time* nil))

(defun eshell/send-input ()
  (interactive)
  (setq *last-command-time* (float-time))
  (eshell-send-input))

(defun contract-eshell-pwd ()
  (let* ((cwd  (eshell/pwd))
	 (dirs (split-string cwd "/"))
	 (result ""))
    (if (> (length dirs) 2)
	(progn
	  (dolist (dir (butlast dirs))
	    (let ((first-letter (if (equal dir "") "" (substring dir 0 1))))
	      (setq result (concat result first-letter "/" ))))
	  (setq result (concat result (car (last dirs)))))
      (setq result cwd))
    result))

(defmacro eshell/prompt-start ()
  `(propertize "┌─" 'face `(:foreground "#AFD75F")))

(defmacro eshell/prompt-section (char condition)
  (let ((res (gensym "res")))
    `(when-let ((,res ,condition))
       (concat
	(propertize "[" 'face `(:foreground "#AFD75F"))
	(propertize (format "%s %s" ,char ,res) 'face `(:foreground "#18aed4"))
	(propertize "]──" 'face `(:foreground "#AFD75F"))))))

(defmacro eshell/prompt-end ()
  `(concat
    (propertize "[" 'face `(:foreground "#AFD75F"))
    (propertize (concat (contract-eshell-pwd)) 'face `(:foreground "#d0d0d0"))
    (propertize "]\n" 'face `(:foreground "#AFD75F"))
    (propertize "└─>" 'face `(:foreground "#AFD75F"))
    (propertize (if (= (user-uid) 0)
		    " # "
		  (format " %s " *eshell-prompt-icon*))
		'face `(:foreground "#AFD75F"))))

(defun git-modified ()
  (let ((args '("diff-index" "--name-only" "HEAD"))
	(result nil))
    (with-temp-buffer
      (apply #'process-file "git" nil (list t nil) nil args)
      (unless (bobp)
	(goto-char (point-min))
	(setq result
	      (buffer-substring-no-properties (point) (line-end-position)))))
    (if result "✗" "✓")))

(defun git-prompt-branch-name ()
  "Get current git branch name"
  (let ((args '("symbolic-ref" "HEAD" "--short")))
    (with-temp-buffer
      (apply #'process-file "git" nil (list t nil) nil args)
      (unless (bobp)
        (goto-char (point-min))
        (buffer-substring-no-properties (point) (line-end-position))))))

(defun git-prompt-status ()
  (when-let ((branch (git-prompt-branch-name)))
    (concat
     branch
     " "
     (git-modified))))

;;(defun git-status ()


(setq eshell-prompt-function
      (lambda ()
	(let ((git   (git-prompt-branch-name)))
	  (concat
	   (eshell/prompt-start)
	   (eshell/prompt-section
	    ;; maybe pull this into its own function bro
	    "" (when *last-command-time*
		 (let* ((total-time (/ (- (float-time)
					  *last-command-time*)
				       60))
			(minutes (truncate total-time)))
		   (format "%s:%s"
			   minutes
			   (truncate (* (- total-time minutes)
				60))))))
	   (eshell/prompt-section "ⓖ" (git-prompt-status))
	   (eshell/prompt-end)))))

(setq eshell-prompt-regexp (format "^[^%s]+ %s " *eshell-prompt-icon* *eshell-prompt-icon*))
