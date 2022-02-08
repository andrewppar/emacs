(load! "~/.emacs.d/keyboard.el")

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/Library/TeX/texbin/")
(setenv "PATH" (mapconcat 'identity exec-path ":"))

;; TODO: Create module! macro that is a thin wrapper around use-package
;; but that makes defining major mode and default bindings for a mode easy
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
       (or (mode . python-mode)
	   (directory . "/Users/andrewparisi/Documents/python")
	   (name . "\*Python\*")))
      ("clojure"
       (or (mode . clojure-mode)
	   (directory . "/Users/andrewparisi/Documents/clojure")
	   (name . "\*cider\*")))
      ("R"
       (or (mode . ess-r-mode)))
      ("magit"
       (name . "*magit*"))
      ("help"
       (or (name . "\*Help\*")
	   (name . "\*Apropos\*")
	   (name . "\*info\*")))
      ("organizer"
       (or (mode . org-mode)
	   (name . "*Org Agenda*")
	   (name . "*Todays Task Log*")))
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

(module! github-review
  :ensure t)

(module! eshell
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
  :ensure t)

                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Figure out a way
;; to make module just a wrapper
;; and not use use-pacage.

(module! recentf-mode
  :use-package nil
  (recentf-mode))

(module! quelpa
  :ensure t)

(module! projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-completion-system 'ivy
        projectile-switch-project-action 'projectile-dired
	projectile-sort-order 'recentf))

(module! envrc
  :ensure t
  :init
  (envrc-global-mode))

(module! org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq *project* "DPS")
  :config

  (defun org-generate-pr-url (number)
    (interactive "sPR Number: ")
    (let* ((project-map '(("ZEM" . "prefect-enrollment-prediction")))
	   (projects    (mapcar 'car project-map))
	   (project     (ido-completing-read
			 "Select Project: " projects))
	   (url (format
		 "https://github.com/reifyhealth/%s/pull/%s"
		 (alist-get project project-map nil nil #'equal)
		 number)))
      (insert url)))

  (defun org-insert-jira-link ()
    (interactive)
    (let ((project nil)
	  (number  nil))
      (if *project*
	  (setq project *project*)
	(setq project
	      (read-from-minibuffer "Project: ")))
      (setq number (read-from-minibuffer "Number: "))
      (insert (format
               "[[https://reifyhealth.atlassian.net/browse/%s-%s][%s-%s]]"
               project number project number)) ))


  (defun my-org-archive-done-tasks ()
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file))

  (defun org-insert-code-block (name language results)
    (interactive "sName: \nsLanguage: \nsResults: ")
    (insert (format "#+NAME: %s\n" name))
    ;; TODO: Make this more like a builder
    (if (equal results "")
	(insert (format "#+BEGIN_SRC %s\n\n" language))
      (insert (format "#+BEGIN_SRC %s :results %s\n\n" language results)))
    (forward-line)
    (insert (format "#+END_SRC\n"))
    (forward-line -2))

  (defun setup-org-file (title)
    (interactive "sTitle: ")
    (let ((date (format-time-string "%m-%d-%Y")))
      (save-excursion
  	(goto-char 0)
  	(insert (format "#+title: %s\n" title))
  	(insert (format "#+date: %s\n" date))
  	(insert (format "#+author: Andrew Parisi\n")))))

  (defun setup-meetings-file (meeting-title)
    (interactive "sTitle: ")
    (let ((date (format-time-string "%b %d, %Y")))
      (setup-org-file meeting-title)
      (goto-char (point-max))
      (insert "#+OPTIONS: toc:nil\n")
      (save-excursion
	(insert "\n")
	(insert "* Date\n")
	(insert date)
	(insert "\n\n")
	(insert "* Participants\n")
	(insert "\n")
	(insert "* Goals\n")
	(insert "\n")
	(insert "* Discussion Topics\n")
	(insert "\n")
	(insert "* Action Items\n")
	(insert "\n")
	(insert "* Decisions\n")
	(insert "\n"))))

  (defun org-task-goto-general ()
    (interactive)
    (goto-char (org-goto-heading "General" '("Tasks"))))

  (defun org-task-goto-jira ()
    (interactive)
    (goto-char (org-goto-heading "JIRA" '("Tasks"))))

  (defun org-task-goto-avicenna ()
    (interactive)
    (goto-char (org-goto-heading "Avicenna" '("Tasks" "JIRA"))))

  (defun org-task-goto-zem ()
    (interactive)
    (goto-char (org-goto-heading "Zero Enroller Model" '("Tasks" "JIRA"))))

  (defun org-jira-link-todo ()
    (interactive)
    (save-excursion
      (end-of-line)
      (let ((end (point)))
        (beginning-of-line)
        (re-search-forward " ")
        (let* ((start       (point))
               (todo-string (buffer-substring start end)))
          (cond ((string-match (regexp-quote "TODO") todo-string 0)
                 (re-search-forward " ")
                 (insert " "))
                ((string-match (regexp-quote "IN PROGRESS") todo-string 0)
                 (re-search-forward " " nil nil 2)
                 (insert " ")))
          (backward-char)
          (org-insert-jira-link)
	  (insert ":")))))

  (defun org-meeting-insert-speaker (speaker)
  (interactive "sSpeaker: ")
  (let ((time (format-time-string "%H:%M")))
    (insert (format "** %s %s\n\n" speaker time))))

  (major-mode-map org-mode
    :bindings
    ("a"   'org-agenda
     "t"   'org-todo
     "ct"  'my-org-archive-done-tasks
     "cs"  'org-archive-subtree
     "jo"  'org-open-at-point
     "fi"  'setup-org-file
     "fmi" 'setup-meetings-file
     "fms" 'org-meeting-insert-speaker
     "ic"  'org-insert-code-block
     "ij"  'org-insert-jira-link
     "it"  'org-jira-link-todo
     "e"   'org-export-dispatch
     "s"   'org-schedule
     "p"   'org-generate-pr-url
     "mp"  'org-move-subtree-up
     "mn"  'org-move-subtree-down
     "mj"  'org-move-item-up
     "mk"  'org-move-item-down
     "mh"  'org-promote-subtree
     "ml"  'org-demote-subtree
     "di"  'org-toggle-inline-images)
    :labels
    ("i"  "insert"
     "j"  "jump"
     "m"  "move"
     "d"  "dial"
     "c"  "clear"
     "fm" "org-meeting-file"
     "f"  "org-file"))
  (evil-define-key 'normal org-mode-map
    (kbd "<tab>") 'org-cycle)
  ;;SLOW
  (require 'ox-md)
  (require 'ox-ipynb)
  (setq org-startup-indented t
  	org-startup-truncated nil
  	org-hide-leading-stars nil
  	org-directory "~/org"
  	org-log-done t
  	org-todo-keywords
  	'((sequence "TODO" "IN PROGRESS" "|" "DONE" "WONT DO(@)"))
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
	   "*** TODO [[https://reifyhealth.atlassian.net/browse/%^{Project}][%\\1]]: %?\nSCHEDULED: %^t")
  	  ("a" "avicenna" entry
  	   (file+function "~/org/status.org" org-task-goto-avicenna)
	   "**** TODO [[https://reifyhealth.atlassian.net/browse/%^{Project}][%\\1]]: %?\nSCHEDULED: %^t")
	  ("z" "zem" entry
  	   (file+function "~/org/status.org" org-task-goto-zem)
	   "**** TODO [[https://reifyhealth.atlassian.net/browse/%^{Project}][%\\1]]: %?\nSCHEDULED: %^t")

	  )
	  org-plantuml-jar-path
	  (expand-file-name "/Users/andrewparisi/Documents/java/jars/plantuml.jar"))

	;; SLOW
	(org-babel-do-load-languages
	 'org-babel-load-languages
	 '((python . t)
	   (clojure . t)
	   (haskell . t)
	   (emacs-lisp . t)
	   (sql . t)
	   (dot . t)
	   (plantuml . t)
	   (shell . t)))
	)

(module! org-agenda
  :requires evil
  :after org
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
    "." 'org-agenda-goto-today
    "e" 'org-agenda-set-effort
    (kbd "<RET>") 'org-agenda-goto
    ">" 'org-agenda-date-prompt)
  (setq
   org-agenda-overriding-columns-format
   "%TODO %7EFFORT %PRIORITY     %100ITEM 100%TAGS"
   org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t%-6e% s")
                              (todo . " %i %-12:c %-6e")
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c"))
   calendar-latitude 42.2
   calendar-longitude -71.0
   calendar-location-name "Quincy, MA"))

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

  ;; clojure
  (add-hook 'clojure-mode-hook #'lsp)
  (add-hook 'clojurec-mode-hook #'lsp)
  (add-hook 'clojurescript-mode-hook #'lsp)

  ;; R
  (add-hook 'ess-r-mode-hook #'lsp)

  ;;yaml
  (add-hook 'yaml-mode-hook #'lsp)

  ;; python
  (add-hook 'python-mode-hook #'lsp)
  :config
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)))
  )

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

(module! flycheck
  :ensure t
  :defer t
  :init
  (global-flycheck-mode)
  :config
  (flycheck-define-checker
      python-mypy ""
      :command ("mypy"
		"--ignore-missing-imports" "--fast-parser"
		source-original)
      :error-patterns
      ((error line-start (file-name) ":" line ": error:" (message) line-end))
      :modes python-mode)
  (add-to-list 'flycheck-checkers 'python-mypy t)
  (flycheck-add-next-checker 'python-pylint 'python-mypy t))

(module! company
  :ensure t
  :defer t
  :requires evil)

            ;;;
;;;;;;;;;;;;;;;

;;;;;;;;;;
;;; python

(module! sphinx-doc
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  ;; TODO: Consider writing a wrapper around
  ;; sphinx-doc that goes to the beginning of
  ;; the current fn.
  )

(module! python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :init
  (add-hook
   'inferior-python-mode-hook
   (lambda ()
     (progn
       (evil-define-key 'normal 'evil-normal-state-map (kbd "C-j") 'comint-next-input)
       (evil-define-key 'insert 'evil-insert-state-map (kbd "C-j") 'comint-next-input)
       (evil-define-key 'normal 'evil-normal-state-map (kbd "C-k") 'comint-previous-input)
       (evil-define-key 'insert 'evil-insert-state-map (kbd "C-k") 'comint-previous-input))))
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args "--simple-prompt -i --InteractiveShell.display_page=True"
   flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

  (add-hook 'python-mode-hook (lambda ()
                                (require 'sphinx-doc)
                                (sphinx-doc-mode t)))
  (major-mode-map python-mode
    :labels
    ("d" "doc"
     "s" "send")
    :bindings
    ("dg" 'google-doc
     "ds" 'sphinx-doc
     "b"  'blacken-buffer
     "g"  'lsp-find-definition
     "sb" 'python-shell-send-buffer
     "sd" 'python-shell-send-defun)))

(module! blacken
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq blacken-line-length '79))

(module! conda
  :ensure t
  :requires evil
  :init
  (conda-env-initialize-eshell)
  :config
  (defun conda-env-switch ()
    (interactive)
    (conda-env-deactivate)
    (conda-env-activate))

  (setq
   conda-anaconda-home "/Users/andrewparisi/anaconda3"
   conda-env-home-directory "/Users/andrewparisi/anaconda3"))

;; (quelpa '(ox-ipynb :fetcher github-ssh :repo "jkitchin/ox-ipynb"))

                 ;;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;
;;; clojure

(module! cider
  :ensure t
  :mode ("\\.clj\\'" . clojure-mode)
  :requires evil
  :init
  ;; If necessary, add more calls to `define-key' here ...
  :config
  (setq cider-repl-pop-to-buffer-on-connect nil
	cider-test-show-report-on-success t
	cider-show-error-buffer nil))

(module! clojure-mode
  :ensure t
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
     "l" "cider load"
     "j"  "repl"))
  :config
  (setq lsp-clojure-server-command '("clojure-lsp")
	org-babel-clojure-backend 'cider))

           ;;;
;;;;;;;;;;;;;;

;;;;;;;;;;;
;;; haskell

(module! haskell-mode
  :ensure t
  :requires (evil which-key)

  :mode ("\\.hs\\'" . haskell-mode)
  :config
  (require 'ob-haskell))

            ;;;
;;;;;;;;;;;;;;;

;;;;;
;;; R

(module! ess
  :ensure t
  :init
  (require 'ess-site)
  (add-hook
   'inferior-ess-r-mode-hook
   (lambda ()
     (progn
       (evil-define-key 'normal 'evil-normal-state-map (kbd "C-j") 'comint-next-input)
       (evil-define-key 'insert 'evil-insert-state-map (kbd "C-j") 'comint-next-input)
       (evil-define-key 'normal 'evil-normal-state-map (kbd "C-k") 'comint-previous-input)
       (evil-define-key 'insert 'evil-insert-state-map (kbd "C-k") 'comint-previous-input))))
  :config
  (setq ess-r-backend 'lsp))

            ;;;
;;;;;;;;;;;;;;;


;;;;;;;;
;;; mail

(module! mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e/"
  :config
  (setq mu4e-mu-binary (executable-find "mu")
	mu4e-maildir "~/.maildir"
	mu4e-drafts-folder "/Users/andrewparisi/.maildir/drafts"
	mu4e-sent-folder   "/Users/andrewparisi/.maildir/sent"
	mu4e-trash-folder  "/Users/andrewparisi/.maildir/trash"
	mu4e-get-mail-command (concat
			       (executable-find "mbsync")
			       " -a")
	mu4e-update-interval 300
	mu4e-attachment-dir "~/Desktop"
	mu4e-change-filename-when-moving t
	mu4e-user-mail-address-list '("andrew.parisi@reifyhealth.com")
	mu4e-confirm-quit nil)
  ;; sending emails
  (require 'smtpmail)
  ;;(require 'epa-file)
  ;;(epa-file-enable)
  (auth-source-forget-all-cached)
  (setq epa-pinentry-mode 'loopback
	message-kill-buffer-on-exit t
	send-mail-function 'sendmail-send-it
	message-send-mail-function 'sendmail-send-it
	sendmail-program (executable-find "msmtp"))

  (defun timu/set-msmtp-account ()
    (if (message-mail-p)
	(save-excursion
          (let*
              ((from (save-restriction
                       (message-narrow-to-headers)
                       (message-fetch-field "from")))
               (account "gmail"))
            (setq message-sendmail-extra-arguments (list '"-a" account))))))
  (add-hook 'message-send-mail-hook 'timu/set-msmtp-account)

  ;; mu4e cc & bcc
  ;; this is custom as well
  (add-hook 'mu4e-compose-mode-hook
            (defun timu/add-cc-and-bcc ()
              "My Function to automatically add Cc & Bcc: headers.
    This is in the mu4e compose mode." (save-excursion (message-add-header "Cc:\n"))
              (save-excursion (message-add-header "Bcc:\n"))))

  ;; mu4e address completion
  (add-hook 'mu4e-compose-mode-hook 'company-mode)

  ;; For some reason this is throwing an error
 ;; (major-mode-map mu4e-view-mode
 ;;   :bindings
 ;;   ("c" 'mu4e-org-store-and-capture))
  )

             ;;;;
;;;;;;;;;;;;;;;;;

;;;;;;;
;;; SQL

(module! sqlformat
  :ensure t
  :config
  (setq sqlformat-command 'pgformatter
	sqlformat-args '("-s2" "-g")))

(module! sql
  :defer t
  :init
  (defun sql-get-password (key account)
    (let ((command (concat  "security "
			    "find-generic-password "
			    "-s '"
			    key
			    "' -a '"
			    account
			    "' -w")))
      (->> command shell-command-to-string split-string car)))

  (setq sql-postgres-login-params nil
	sql-connection-alist
	'((psql-prod-concept-data
	   (sql-product 'postgres)
	   (sql-database
	    (concat
	     "postgresql://"
	     "postgres"
	     ":"
	     (sql-get-password
	      "postgresql://concept-data-production.cxyq5v2k4dfd.us-east-1.rds.amazonaws.com"
	      "postgres")
	     "@concept-data-production.cxyq5v2k4dfd.us-east-1.rds.amazonaws.com"
	     ":5432"
	     "/concept_data")))
          (psql-prod-dev
	   (sql-product 'postgres)
	   (sql-database
	    (concat
	     "postgresql://"
	     "postgres"
	     ":"
	     (sql-get-password
	      "postgresql://concept-data-production.cxyq5v2k4dfd.us-east-1.rds.amazonaws.com"
	      "postgres")
	     "@concept-data-production.cxyq5v2k4dfd.us-east-1.rds.amazonaws.com"
	     ":5432"
	     "/development")))
          (psql-dev-cd
	   (sql-product 'postgres)
	   (sql-database
	    (concat
	     "postgresql://"
	     "postgres"
	     ":"
	     (sql-get-password
	      "postgresql://concept-data-testing.cncpevj1rbhb.us-east-1.rds.amazonaws.com"
	      "postgres")
	     "@concept-data-testing.cncpevj1rbhb.us-east-1.rds.amazonaws.com"
	     ":5432"
	     "/concept_data")))
	  (psql-aact
	   (sql-product 'postgres)
	   (sql-database
	    (concat
	     "postgresql://"
	     "aparisi"
	     ":"
	     ;; TODO: put this in the secrets
	     (url-hexify-string "JYM@wcd_gkp.aug0adb")
	     "@aact-db.ctti-clinicaltrials.org"
	     ":5432"
	     "/aact")))
	  (redshift-dw-dev
	   (sql-product 'postgres)
	   (sql-database
	    (concat
	     "postgresql://"
	     "dev"
	     ":"
	     (sql-get-password
	      "postgresql://localhost:5439"
	      "dev")
	     "@localhost"
	     ":5439"
	     "/development")))))
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (toggle-truncate-lines t)))


  (defun sql-add-newline-first (output)
    "Add newline to beginning of OUTPUT for `comint-preoutput-filter-functions'"
    (concat "\n" output))

  (evil-define-key 'insert sql-mode-map (kbd "C-c p") 'autocomplete-table)

  ;;(defun sqli-add-hooks ()
  ;;  "Add hooks to `sql-interactive-mode-hook'."
  ;;  (add-hook 'comint-preoutput-filter-functions
  ;;            'sql-add-newline-first))

  ;;(add-hook 'sql-interactive-mode-hook 'sqli-add-hooks)
  )



           ;;;
;;;;;;;;;;;;;;

;;;;;;;
;;; CSV

(module! csv-mode
  :ensure t
  :config
  (setq csv-separators '("," "    "))
  (add-hook 'csv-mode-hook
            (lambda ()
              (define-key csv-mode-map (kbd "C-c C-M-a")
		(defun csv-align-visible (&optional arg)
                  "Align visible fields"
                  (interactive "P")
                  (csv-align-fields
		   nil
		   (window-start)
		   (window-end)))))))



       ;;;
;;;;;;;;;;

;;;;;;;;;;;;;
;;; Utilities

(module! yaml-mode
   :ensure t)

;;;;;;;;;;;;;
;;; Semantics

(module! ttl-mode
  :ensure t)

(module! sparql-mode
  :ensure t)

;;;
;;;;;;;;;;;;;;
