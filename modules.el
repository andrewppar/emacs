(load! "~/.emacs.d/keyboard.el")

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
  :config (counsel-mode))

(module! ivy
  :ensure t
  :requires evil
  :config
  (setq ivy-height 10
		 ivy-use-virtual-buffers t
		 ivy-count-format "(%d/%d) "
		 ivy-initial-inputs-alist nil
		 ivy-re-builders-alist
		 '((t . ivy--regex-ignore-order)))
  (ivy-mode 1))

(module! ivy-rich
  :ensure t
  :after (:all ivy counsel)
  :config
  (setq ivy-virtual-abbreviate 'full
	ivy-rich-switch-buffer-align-virtual-buffer t
	ivy-rich-path-style 'abbrev)
  (ivy-rich-mode))

(module! swiper
  :ensure t
  :after ivy)

(module! evil-collection
  :after evil
  :ensure t
  :init
  :config
  (evil-collection-init)
  (setq evil-collection-magit-use-z-for-folds t))

                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default Loaded Packages

(module! ibuffer
  :requires evil
  :config
  (setq
   ibuffer-saved-filter-groups
   '(("default"
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
	   (mode . shell-mode)))))
   evil-emacs-state-modes (delq
			   'ibuffer-mode
			   evil-emacs-state-modes)
   ibuffer-exper t
   ibuffer-show-empty-filter-groups nil)
  (add-hook 'ibuffer-mode-hook
	    '(lambda ()
	       (ibuffer-switch-to-saved-filter-groups
		"default"))))

(module! magit
  :ensure t
  :requires evil
  :defer t
  :config
  (setq
   magit-display-buffer-function
   #'magit-display-buffer-fullframe-status-v1
   ediff-window-setup-function
   #'ediff-setup-windows-plain)
  
  (defun git-commit-message-setup ()
    (insert (format "%s " (magit-get-current-branch))))

  (major-mode-map magit-mode
    :bindings
    ("" 'magit-dispatch))

  )

(module! eshell
  :init
  (evil-define-key 'normal 'eshell-mode-map
    (kbd "C-j") 'eshell-next-input
    (kbd "C-k") 'eshell-previous-input)
  (evil-define-key 'insert 'eshell-mode-map
    (kbd "C-j") 'eshell-next-input
    (kbd "C-k") 'eshell-previous-input)
  (major-mode-map eshell-mode
    (:bindings
     "c" 'eshell/clear)))

(module! projectile
  :ensure t
  :defer t 
  :init
  (projectile-mode +1)
  :config
  (setq projectile-completion-system 'ivy
        projectile-switch-project-action 'projectile-dired
	projectile-sort-order 'recentf))

                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module! ag
  :ensure t)

;;;;;;;
;;; org

(module! org
  :ensure t
  :defer t
  :requires evil
  :init
  (add-hook 'org-mode-hook 'turn-on-flyspell)
  :config
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
	org-babel-clojure-backend 'cider)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (clojure . t)
     (emacs-lisp . t)
     (shell . t))))

(module! org-agenda
  :requires evil
  :defer t
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
       ;;;
;;;;;;;;;;

;;;;;;;;;;;
;;; clojure

(module! cider				;
  :ensure t
  :requires evil
  :defer t
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

(module! clojure-mode
  :ensure t
  :defer t
  :requires (evil which-key)
  :init
  (major-mode-map clojure-mode
    :bindings
    ("jj" 'cider-jack-in
     "jc" 'cider-connect-clj
     "jq" 'cider-quit
     "n"  'cider-repl-set-ns
     "q"  'cider-quit
     "e"  'cider-eval-bufer
     "s"  'cider-toggle-trace-var
     "g"  'xref-find-definitions
     "c"  'cider-eval-defun-at-point
     "a"  'lsp-execute-code-action)
    :labels
    (""  "major mode"
     "j"  "repl"))
  (epa-file-disable)
  :config
  (evil-define-key 'normal 'cider-mode-map
    (kbd "C-k") 'cider-repl-previous-input
    (kbd "C-j") 'cider-repl-next-input)
  (evil-define-key 'insert 'cider-mode-map
    (kbd "C-k") 'cider-repl-previous-input
    (kbd "C-j") 'cider-repl-next-input))

          ;;;
;;;;;;;;;;;;;

;;;;;;;;;;
;;; python

(module! python
  :ensure t
  :defer t
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

(module! blacken
  :ensure t
  :defer t
  :hook (python-mode . blacken-mode)
  :config
  (setq blacken-line-length '79))

(module! conda
  :ensure t
  :defer t
  :requires evil
  :config
  (setq
   conda-anaconda-home "/Users/andrew/anaconda3"
   conda-env-home-directory "/Users/andrew/anaconda3"))

      ;;;
;;;;;;;;;

;;;;;;;;
;;; IDE

(module! flycheck
  :ensure t
  :defer t
  :config
  (global-flycheck-mode))

(module! lsp-mode
  :ensure t
  :defer t
  :init
  (setq lsp-clojure-server-command '("clojure-lsp")
 	lsp-enable-indentation nil
 	lsp-enable-completion-at-point nil
	indent-region-function #'clojure-indent-function)
  (add-hook 'clojure-mode-hook #'lsp)
  (add-hook 'clojurec-mode-hook #'lsp)
  (add-hook 'clojurescript-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  :config
  (require 'lsp-clojure)
  (add-to-list 'lsp-language-id-configuration '(clojure-mode . "clojure"))
  (add-to-list 'lsp-language-id-configuration '(clojurec-mode . "clojure"))
  (add-to-list 'lsp-language-id-configuration '(clojurescript-mode . "clojurescript")))

(module! lsp-ui
  :ensure t
  :defer t
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
  :defer t
  :requires evil)

    ;;;
;;;;;;;

;;;;;;;;
;;; mail

(module! mu4e
  :load-path  "/usr/local/share/emacs/site-lisp/mu/mu4e/"
  :config
  (setq mu4e-mu-binary (executable-find "mu")
	mu4e-maildir "~/.maildir"
	mu4e-get-mail-command
	(concat (executable-find "mbsync") " -a")
	mu4e-update-interval 180
	mu4e-attachment-dir "~/Desktop"
	mu4e-change-filenames-when-moving t
	user-mail-address "andrew.p.parisi@gmail.com"
	mu4e-sent-messages-behavior 'delete


	user-full-name "Andrew Parisi"
	mu4e-compose-signature
	"Andrew Parisi"
	mu4e-show-images t
	mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")
	
  ;;Custom Bindings
  (define-key mu4e-view-mode-map (kbd "j") 'next-line)
  (define-key mu4e-view-mode-map (kbd "k") 'previous-line)
  (define-key mu4e-headers-mode-map (kbd "j") 'next-line)
  (define-key mu4e-headers-mode-map (kbd "k") 'previous-line)
  (define-key mu4e-main-mode-map (kbd "U") 'mu4e-update-index))

(setf epa-pinentry-mode 'loopback)

     ;;;
;;;;;;;;
