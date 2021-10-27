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
  :diminish
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
	       (ibuffer-switch-to-saved-filter-groups "default"))))

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

                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
