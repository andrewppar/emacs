(load "~/.emacs.d/core.el")


;; Required Packages
(modules!
 (evil-collection
  :ensure t
  :calls
  ((evil-collection-init))
  )
 (counsel
  :ensure t)
 (ivy
  :ensure t
  :vars
  (ivy-height
   10
   ivy-use-virtual-buffers t
   ivy-count-format ""
   ivy-initial-inputs-alist nil
   ivy-re-builders-alist '((t . ivy--regex-ignore-order))
   ivy-views `(("{} memacs"
		(horz
		 (file "~/.emacs.d/modules.el")
		 (file "~/.emacs.d/keyboard.el")))))
  :calls
  ((ivy-mode 1)))
 (which-key
  :ensure t
  :vars
  (which-key-idle-delay
   0.1
   which-key-separator " → ")
  :calls
  ((which-key-mode))))

;; Default Loaded Packages
(modules!
 (ibuffer
  :ensure nil
  :vars
  (ibuffer-saved-filter-groups
   '(("home"
      ("python" (or (mode . pyhton-mode)
                    (directory . "/Users/andrew/Documents/python")
                    (name . "\*Python\*")))
      ("clojure" (or (mode . clojure-mode)
                     (directory . "/Users/andrew/Documents/clojure")
                     (name . "\*cider\*")))
      ("magit"   (name . "\*magit"))
      ("help"    (or (name . "\*Help\*")
                     (name . "\*Apropos\*")
                     (name . "\*info\*")))
      ("filesystem" (or (mode . dired-mode)
			(mode . shell-mode)))))))
 (org
  :ensure t
  :vars
  (org-startup-indented
   t
   org-hide-leading-stars nil
   org-directory "~/org"
   org-log-done t
   org-todo-keywords
   '((sequence "TODO" "IN PROGRESS" "|" "DONE"))
   org-hide-leading-stars t
   org-confirm-babel-evaluate nil
   org-agenda-files (list "~/org/status.org")
   org-babel-clojure-backend 'cider
   )
  :calls
  ((org-babel-do-load-languages
    'org-babel-load-languages
    '((python . t)
      (clojure . t)
      (emacs-lisp . t)
      (shell . t))))
  )
 (cider
  :ensure t
  :calls
  ((add-to-list 'exec-path "/usr/local/bin"))
  )
 (python
  :ensure t
  :vars
  (python-shell-interpreter
   "ipython"
   python-shell-interpreter-args "--simple-prompt -i"))
 (lsp-mode
  :ensure t
  :hook
  ((python-mode . lsp)
   (clojure-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  )
 (magit
  :ensure t
  :vars
  (magit-display-buffer-function
   #'magit-display-buffer-fullframe-status-v1
   ediff-window-setup-function #'ediff-setup-windows-plain)
  )
 (clojure-mode
  :ensure t
  :vars
  (clojure-indent-style 'always-align))
 (conda
  :ensure t
  :vars
  (conda-anaconda-home "/Users/andrew/anaconda3"))
 )
