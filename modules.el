(load! "~/.emacs.d/keyboard.el")

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/Library/TeX/texbin/")
(setenv "PATH" (mapconcat 'identity exec-path ":"))

;; TODO: Create module! macro that is a thin wrapper around use-package
;; but that makes defining major mode and default bindings for a mode easy
;; TODO: Figure a way to auto configure lsp mode for a language
;;;;;;;;;;;;;;;;;;;;
;; Required Packages

;; TODO See whether or not these can be paired down

(module! undo-tree
  :ensure t
  :requires evil
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))

(module! counsel
  :ensure t
  :requires evil
  )

(module! ivy
  :ensure t
  :requires evil
  :config
  (setq ivy-height 10
		 ivy-use-virtual-buffers t
		 ivy-count-format ""
		 ivy-initial-inputs-alist nil
		 ivy-re-builders-alist
		 '((t . ivy--regex-ignore-order)))
  (ivy-mode 1))

                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default Loaded Packages

(module! ibuffer
  :requires evil
  :config
  (setq
   ibuffer-saved-filter-groups
   '(("home"
      ("python"
       (or (mode . pyhton-mode)
	   (directory . "/Users/andrewparisi/Documents/python")
	   (name . "\*Python\*")))
      ("clojure"
       (or (mode . clojure-mode)
	   (directory . "/Users/andrewparisi/Documents/clojure")
	   (name . "\*cider\*")))
      ("magit"
       (name . "\*magit"))
      ("help"
       (or (name . "\*Help\*")
	   (name . "\*Apropos\*")
	   (name . "\*info\*")))
      ("filesystem"
       (or (mode . dired-mode)
	   (mode . shell-mode))))))
  (setq evil-emacs-state-modes
	(delq 'ibuffer-mode evil-emacs-state-modes)))

(module! org
  :ensure t
  :requires evil
  :init
  (defun org-insert-jira-link (project number)
  (interactive "sProject: \nsNumber: \n")
  (insert (format
           "[[https://reifyhealth.atlassian.net/browse/%s-%s][%s-%s]]"
           project number project number)))
  
  (defun my-org-archive-done-tasks ()
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file))

  (defun org-insert-code-block (language settings)
    (interactive "sLanguage: \nsSettings: ")
    (insert (format "#+begin_src %s %s\n\n" language settings))
    (forward-line)
    (insert (format "#+end_src\n"))
    (forward-line -2))

  (defun setup-org-file (title)
  (interactive "sTitle: ")
  (let ((date (format-time-string "%m-%d-%Y")))
    (save-excursion
      (goto-char 0)
      (insert (format "#+title: %s\n" title))
      (insert (format "#+date: %s\n" date))
      (insert (format "#+author: Andrew Parisi\n")))))

  (defun org-task-goto-general ()
    (interactive)
    (goto-char (org-goto-heading "General" '("Tasks"))))

  (defun org-task-goto-jira ()
    (interactive)
    (goto-char (org-goto-heading "JIRA" '("Tasks"))))

  (major-mode-map org-mode
    :bindings
    ("a"  'org-agenda
     "t"  'org-todo
     "s"  'my-org-archive-done-tasks
     "fi" 'setup-org-file
     "ic" 'org-insert-code-block
     "ij" 'org-insert-jira-link
     "it" 'org-jira-link-todo
     "e"  'org-export-dispatch
     "c"  'org-capture)
    :labels
    ("i"  "insert"
     "f"  "org-file"))
  (evil-define-key 'normal org-mode-map
    (kbd "<tab>") 'org-cycle)
  :config
  (require 'ox-md)
  (setq org-startup-indented t
	org-startup-truncated nil
	org-hide-leading-stars nil
	org-directory "~/org"
	org-log-done t
	org-todo-keywords
	'((sequence "TODO" "IN PROGRESS" "|" "DONE"))
	org-hide-leading-stars t
	org-confirm-babel-evaluate nil
	org-agenda-files (list "~/org/status.org")
	org-capture-default-notes-file "~/org/status.org"
	org-capture-templates
	'(("g" "general" entry
	   (file+function "~/org/status.org" org-task-goto-general)
	   "*** TODO %?\nSCHEDULED: %^t")
	  ("j" "jira" entry
	   (file+function "~/org/status.org" org-task-goto-jira)
	   "*** TODO %?\nSCHEDULED: %^t"))
	org-babel-clojure-backend 'cider)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (clojure . t)
     (haskell . t)
     (emacs-lisp . t)
     (sql . t)
     (shell . t))))

(module! org-agenda
  :requires evil
  :config
  (evil-set-initial-state 'org-agenda-mode 'normal)
   (evil-define-key 'normal org-agenda-mode-map
     "q" 'org-agenda-quit
     "r" 'org-agenda-redo
     "s" 'org-save-all-buffers
     "t" 'org-agenda-todo
     "d" 'org-agenda-day-view
     "w" 'org-agenda-week-view
     "f" 'org-agenda-later
     "b" 'org-agenda-earlier
     "c" 'org-capture
     (kbd "<RET>") 'org-agenda-goto
     ">" 'org-agenda-date-prompt))

(module! cider				;
  :ensure t
  :requires evil
  :config
  (defun cider-show-cider-buffer ()
    "Shows the nrepl buffer, but does not focus it."
    (interactive)
    (command-execute 'cider-switch-to-repl-buffer)
    (command-execute 'cider-switch-to-last-clojure-buffer))
  
  (defun clojure-set-up-key-bindings ()
    (define-key clojure-mode-map (kbd "C-c r") 'cider-repl))
  ;; If necessary, add more calls to `define-key' here ...
  (with-eval-after-load 'cider
    (evil-define-key 'insert cider-repl-mode-map
      (kbd "C-j") 'cider-repl-next-input
      (kbd "C-k") 'cider-repl-previous-input))
  (major-mode-map cider-repl-mode
    :bindings
    ("c" 'cider-repl-clear-buffer
     "k" 'cider-repl-previous-input
     "j" 'cider-repl-next-input))
  (add-hook 'cider-repl-mode-hook 'clojure-set-up-key-bindings)
  
  
  (setq cider-repl-pop-to-buffer-on-connect nil
	cider-show-error-buffer nil))

(module! python
  :ensure t
  :requires evil
  :config
  (major-mode-map python-mode
    :bindings
    ("sd" 'python-shell-send-defun
     "sb" 'python-shell-send-buffer)
    :labels
    ("s"  "send"))
  (evil-define-key 'normal 'evil-normal-state-map (kbd "C-j") 'comint-next-input)
  (evil-define-key 'insert 'evil-insert-state-map (kbd "C-j") 'comint-next-input)
  (evil-define-key 'normal 'evil-normal-state-map (kbd "C-k") 'comint-previous-input)
  (evil-define-key 'insert 'evil-insert-state-map (kbd "C-k") 'comint-previous-input)
  (setq
    python-shell-interpreter "ipython"
    python-shell-interpreter-args "--simple-prompt -i --InteractiveShell.display_page=True"
    flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(module! lsp-mode
  :init
  (setq lsp-clojure-server-command '("clojure-lsp")
	lsp-enable-indentation nil
	lsp-enable-completion-at-point nil)
  ;; (setq indent-region-function #'clojure-indent-function)
  (add-hook 'clojure-mode-hook #'lsp)
  (add-hook 'clojurec-mode-hook #'lsp)
  (add-hook 'clojurescript-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  :config
  (require 'lsp-clojure)
  (add-to-list 'lsp-language-id-configuration '(clojure-mode . "clojure"))
  (add-to-list 'lsp-language-id-configuration '(clojurec-mode . "clojure"))
  (add-to-list 'lsp-language-id-configuration '(clojurescript-mode . "clojurescript")))

(module! flycheck
  :ensure t 
  :init 
  (global-flycheck-mode))

(module! lsp-ui
  :ensure t
  :after (lsp-mode)

  :init (setq lsp-ui-doc-enable t
              lsp-ui-doc-use-webkit t
              lsp-ui-doc-header t
              lsp-ui-doc-delay 0.2
              lsp-ui-doc-include-signature t
              lsp-ui-doc-alignment 'at-point
              lsp-ui-doc-use-childframe t
              lsp-ui-doc-border (face-foreground 'default)
              lsp-ui-peek-enable t
              lsp-ui-peek-show-directory t
	      lsp-ui-sideline-show-diagnostics t
              lsp-ui-sideline-enable t
              lsp-ui-sideline-show-code-actions t
              lsp-ui-sideline-show-hover t
              lsp-ui-sideline-ignore-duplicate t)
  :config
  (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'after-load-theme-hook
	    (lambda ()
              (setq lsp-ui-doc-border (face-foreground 'default))
              (set-face-background 'lsp-ui-doc-background
				   (face-background 'tooltip))))

  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; @see https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

(module! company
  :ensure t
  :requires evil)

(module! magit
  :ensure t
  :requires evil
  :config
  (setq
   magit-display-buffer-function
   #'magit-display-buffer-fullframe-status-v1
   ediff-window-setup-function
   #'ediff-setup-windows-plain)
  (major-mode-map magit-mode
    :bindings
    ("" 'magit-dispatch)))

(module! clojure-mode
  :ensure t
  :requires (evil which-key)
  :init
  (major-mode-map clojure-mode
    :bindings
    ("jj" 'cider-jack-in
     "jc" 'cider-connect-clj
     "jq" 'cider-quit
     "s"  'cider-toggle-trace-var
     "g"  'xref-find-definitions
     "c"  'cider-eval-defun-at-point
     "a"  'lsp-execute-code-action)
    :labels
    (""  "major mode"
     "j"  "repl")))

(module! haskell-mode
  :ensure t
  :requires (evil which-key)
  :init
  (require 'ob-haskell))

(module! conda
  :ensure t
  :requires evil
  :config
  (setq
   conda-anaconda-home "/Users/andrewparisi/anaconda3"
   conda-env-home-directory "/Users/andrewparisi/anaconda3")
  (conda-env-initialize-eshell)
  )

(module! eshell
  :init
  (evil-define-key 'normal 'eshell-mode-map
    (kbd "C-j") 'eshell-next-input
    (kbd "C-k") 'eshell-previous-input)
  (evil-define-key 'insert 'eshell-mode-map
    (kbd "C-j") 'eshell-next-input
    (kbd "C-k") 'eshell-previous-input))

(module! telephone-line
  :ensure t
  :config
  (setq telephone-line-primary-right-separator 'telephone-line-abs-left
	telephone-line-secondary-right-separator 'telephone-line-abs-hollow-left
	telephone-line-height 24
	telephone-line-evil-use-short-tag t)
  (telephone-line-mode +1))
