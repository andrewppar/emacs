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
  :diminish
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
      ("help"
       (or (name . "\*Help\*")
	   (name . "\*Apropos\*")
	   (name . "\*info\*")))
      ("emacs"
       (or (mode . emacs-lisp-mode)))
      ("filesystem"
       (or (mode . dired-mode)
	   (mode . eshell-mode)))))
   evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes)
   ibuffer-expert t
   ibuffer-show-empty-filter-groups nil)
  (add-hook 'ibuffer-mode-hook
	    '(lambda ()
	       (ibuffer-switch-to-saved-filter-groups
		"default"))))

(module! magit
  :ensure t
  :config
  (setq
   magit-display-buffer-function
   #'magit-display-buffer-fullframe-status-v1
   ediff-window-setup-function
   #'ediff-setup-windows-plain)

  (defun git-commit-message-setup ()
    (insert (format "%s " (magit-get-current-branch))))

  (add-hook 'git-commit-setup-hook 'git-commit-message-setup)

  (major-mode-map magit-mode
    :bindings
    ("" 'magit-dispatch)))

(module! git-timemachine
  :ensure t
  :defer t)

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
  :init
  (projectile-mode +1)
  :config
  (setq projectile-completion-system 'ivy
        projectile-switch-project-action 'projectile-dired
	projectile-sort-order 'recentf))

                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;
;;; LSP

(module! lsp-mode
  :ensure t
  :hook (prog-mode . display-fill-column-indicator-mode)
  :init
  (setq lsp-enable-indentation nil
	lsp-enable-completion-at-point nil
	lsp-lens-enable t
	lsp-signature-auto-activate nil)

  ;; TODO: Add these to the :hook section
  (add-hook 'clojure-mode-hook #'lsp)
  (add-hook 'clojurec-mode-hook #'lsp)
  (add-hook 'clojurescript-mode-hook #'lsp))

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

     ;;;
;;;;;;;;

;;;;;;;;;;;
;;; Clojure


(module! cider
  :ensure t
  :defer t
  :mode ("\\.clj\\'" . clojure-mode)
  :requires evil
  :init
  ;; If necessary, add more calls to `define-key' here ...
  :config
  (setq cider-repl-pop-to-buffer-on-connect nil
	cider-test-show-report-on-success t
	cider-repl-display-help-banner nil
	cider-show-error-buffer nil))

(module! clojure-mode
  :ensure t
  :defer t
  :mode ("\\.clj\\'" . clojure-mode)
  :requires (evil which-key)
  :init
  (defun my-cider-jack-in ()
    (interactive)
    (my-cider-op 'cider-jack-in '()))

  (defun my-cider-connect ()
    (interactive)
    (my-cider-op 'cider-connect-clj '()))

  (defun my-cider-op (op &rest args)
    (apply op args)
    (major-mode-map cider-repl-mode
      :bindings
      ("c" 'cider-repl-clear-buffer
       "k" 'cider-repl-previous-input
       "j" 'cider-repl-next-input)
      :labels
      ("" "major mode")))

  (major-mode-map clojure-mode
    :bindings
    ("jj" 'my-cider-jack-in
     "jc" 'my-cider-connect
     "jq" 'cider-quit
     "fd" 'cider-format-defun
     "fb" 'cider-format-buffer
     "ld" 'cider-eval-defun-at-point
     "lb" 'cider-load-buffer
     "q"  'cider-quit
     "s"  'cider-toggle-trace-var
     "n"  'cider-repl-set-ns
     "g"  'xref-find-definitions
     "c"  'cider-eval-defun-at-point
     "r"  'lsp-find-references
     "d"  'lsp-describe-thing-at-point
     "tt" 'cider-test-run-project-tests
     "tn" 'cider-test-run-ns-tests
     "a"  'lsp-execute-code-action)
    :labels
    (""  "major mode"
     "f" "format"
     "t" "test"
     "l" "cider load"
     "j"  "repl"))
  :config
  (setq lsp-clojure-server-command '("clojure-lsp")
	org-babel-clojure-backend 'cider))

      ;;;
;;;;;;;;;
