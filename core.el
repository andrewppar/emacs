(require 'package)

(add-to-list 'package-archives 
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives 
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives 
             '("melpa" . "https://melpa.org/packages/") t)

(defun update-core ()
  (setq package-list '(evil use-package))
  
 ; activate all the packages (in particular autoloads)
  (package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package))))


(defun generate-configuration (module)
  (let ((var-plist (plist-get module :vars))
	(result    '()))
    (when var-plist
      (cl-do ((variable (car var-plist) (car rest))
	      (value    (cadr var-plist) (cadr rest))
	      (rest     (cddr var-plist) (cddr rest)))
	  ((not rest)
	   (setq result (cons `(setq ,variable ,value) result)))
	(setq result (cons `(setq ,variable ,value) result)))
      `(:config . ,result))))

(defmacro modules! (&rest modules)
  (let ((result '()))
    (dolist (module modules)
      (if (listp module)
	  (let* ((module-result '())
		 (module-core   (cdr module))
		 (config-block  (generate-configuration module-core))
		 (ensure?       (plist-get module-core :ensure))
		 (ensure-block  (if ensure? '(:ensure t) nil))
		 (call-block    (plist-get module-core :calls))
		 (hook-block   (plist-get module-core :hook))
		 (blocks        ensure-block))
	    (when call-block
	      (if config-block
		  (setq config-block (append config-block call-block))
		(setq config-block (cons :config call-block))))
	    (when config-block
	      (setq blocks (append blocks config-block)))
	    (when hook-block
	      (setq blocks (cons :hook (cons hook-block blocks))))
	    (setq result
		  (cons `(use-package ,(car module) . ,blocks) result)))
	(setq result (cons `(use-package ,module :ensure t) result))))
    (message (format "Modules Loaded: %s" result))
    (cons 'progn result)))

(defmacro ensure-evil-mode! (&rest modes)
  (let ((result '()))
    (dolist (mode modes)
      (push
       `(setq evil-emacs-state-modes (delq ,mode evil-emacs-state-modes))
       result))
    (cons 'progn result)))


