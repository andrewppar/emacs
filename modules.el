(load! "~/.emacs.d/keyboard.el")

;; TODO: Figure a way to auto configure lsp mode for a language
;; TODO: Add native quelpa support and ensure we can call it from
;; module!
;;;;;;;;;;;;;;;;;;;;
;; Required Packages
;; TODO See whether or not these can be paired down

(module! undo-tree
  :ensure t
  :requires evil
  :diminish
  :init
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))

(module! counsel
  :ensure t
  :after ivy
  :requires evil
  :config (counsel-mode))

(module! ivy
  :ensure t
  :defer 0.1
  :diminish
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

(module! dired
  :use-package nil

  (defun dired-goto-and-find ()
    (interactive)
    (let ((file (expand-file-name
		 (read-file-name "Goto file: "
				 (dired-current-directory) nil t))))
      (dired-goto-file file)
      (dired-find-alternate-file)))

  (setq dired-dwim-target t)
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))

  (evil-define-key
    'normal dired-mode-map
    "l" 'dired-find-alternate-file
    "h" 'dired-up-directory
    "f" 'dired-goto-and-find))

(module! pbcopy
  :ensure t
  :init
  (turn-on-pbcopy))

(module! recentf-mode
  :use-package nil
  (recentf-mode))
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
	    #'(lambda ()
	       (ibuffer-switch-to-saved-filter-groups
		"default"))))

(module! magit
  :ensure t
  :defer t
  :config
  (setq
   magit-display-buffer-function
   #'magit-display-buffer-fullframe-status-v1
   ediff-window-setup-function
   #'ediff-setup-windows-plain)

  (defun git-commit-message-setup ()
    (insert (format "%s \n" (magit-get-current-branch))))

  (add-hook 'git-commit-setup-hook 'git-commit-message-setup)

  (major-mode-map magit-mode
    :bindings
    ("" 'magit-dispatch)))

(module! git-timemachine
  :ensure t
  :defer t)

(module! eshell
  :use-package nil
  :init
  (load! "~/.emacs.d/eshell.el")
  (evil-define-key 'normal 'eshell-mode-map
    (kbd "C-j") 'eshell-next-matching-input-from-input
    (kbd "C-k") 'eshell-previous-matching-input-from-input
    (kbd "RET") 'eshell/send-input)
  (evil-define-key 'insert 'eshell-mode-map
    (kbd "C-j") 'eshell-next-matching-input-from-input
    (kbd "C-k") 'eshell-previous-matching-input-from-input
    (kbd "RET") 'eshell/send-input)
  (major-mode-map eshell-mode
    (:bindings
     "c" 'eshell/clear)))

(module! ag
  :defer t
  :ensure t)

;; TODO: Figure out a way
;; to make module just a wrapper
;; and not use use-package.

(module! lsp-mode
  :ensure t
  :hook (prog-mode . display-fill-column-indicator-mode)
  :init
  (setq lsp-enable-indentation nil
	lsp-enable-completion-at-point nil
	lsp-lens-enable t
	lsp-signature-auto-activate nil)
  )

                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
