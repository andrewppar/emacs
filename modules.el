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
  :config
  (setq
   magit-display-buffer-function
   #'magit-display-buffer-fullframe-status-v1
   ediff-window-setup-function
   #'ediff-setup-windows-plain)

  (major-mode-map magit-mode
    :bindings
    ("" 'magit-dispatch)))

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
