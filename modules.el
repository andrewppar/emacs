(load! "~/.emacs.d/keyboard.el")


;;TODO: Are these necessary?
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'load-path "~/.emacs.d/lisp")

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
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
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

(module! pbcopy
  :ensure t
  :init
  (turn-on-pbcopy))

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
      ("magit"
       (name . "*magit*"))
      ("help"
       (or (name . "\*Help\*")
	   (name . "\*Apropos\*")
	   (name . "\*info\*")))
      ("keep"
       (or (name . "*Org Agenda*")
	   (name . "*Todays Task Log*")
	   (name . "status.org")
	   (name . "*scratch*")
	   (name . "*Messages*")
	   (name . "*Eirene Splash*")))
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

(module! dired
  :use-package nil

  (defun dired-get-root ()
    (save-excursion
      (goto-char 0)
      (let ((line (thing-at-point 'line t)))
	(car (split-string line ":" t "[\t \n]+")))))

  (defun dired-path-line? (line)
    (or
     (string-prefix-p "  d" line)
     (string-prefix-p "  -" line)))

  (defun dired-path-name (line)
    (let ((line-list (split-string line " " t "\n")))
      (nth 8 line-list)))

  (defvar *dired-yanked-path* nil)

  (defun dired-yank-item ()
    (interactive)
    (let ((root (dired-get-root))
	  (line (thing-at-point 'line t)))
      (when (dired-path-line? line)
	(let ((full-path (format "%s/%s" root (dired-path-name line)))
	      (directory? (string-prefix-p "  d" line)))
	  (kill-new full-path)
	  (setq *dired-yanked-path* `(,full-path . ,directory?))
	  (message (format "Yanked %s" full-path))
	  ))))

  (defun dired-paste-item ()
    (interactive)
    (when *dired-yanked-path*
      (let ((current-dir (format "%s/" (dired-get-root))))
	(if (cdr *dired-yanked-path*)
	    (copy-directory (car *dired-yanked-path*) current-dir)
	  (copy-file (car *dired-yanked-path*) current-dir)
	  (message (format "Copied %s to %s" copy-file current-dir))))))

  (defun dired-build-location-map ()
    (let ((result      '())
	  (line-number  1))
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (let ((line (thing-at-point 'line t)))
	    (when (dired-path-line? line)
	      (push
	       (cons (dired-path-name line) line-number)
	       result))
	    (setq line-number (+ line-number 1))
	    (forward-line 1))))
      result))

  (defun dired-goto-and-find ()
    (interactive)
    (let* ((location-map  (dired-build-location-map))
	   (all-locations (mapcar 'car location-map))
	   (location (ivy-completing-read
		      "Navigate to file: " all-locations nil t)))
      (goto-char (point-min))
      (forward-line (- (alist-get location location-map nil nil #'equal) 1))
      (dired-find-alternate-file)))

  (defun dired-open-with ()
    (interactive)
    (let ((line (thing-at-point 'line t)))
      (if (not (dired-path-line? line))
	  (message "No path at point")
	(let* ((path (dired-path-name line))
	       (open-command (read-string
			      "Open With Command: " nil nil "open"))
	       (command (format "%s %s" open-command path)))
	  (shell-command command)))))

  (setq dired-dwim-target t)
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))

  (evil-define-key
    'normal dired-mode-map
    "l" 'dired-find-alternate-file
    "h" 'dired-up-directory
    "f" 'dired-goto-and-find
    "w" 'dired-open-with
    "y" 'dired-yank-item
    "p" 'dired-paste-item))

(module! treemacs
  :use-package nil
  :init
  (evil-define-key
    'normal treemacs-mode-map
    "l" 'treemacs-TAB-action
    "+" 'treemacs-create-dir
    "f" 'treemacs-create-file
    ))

(module! pandoc-mode
  :ensure t
  :defer t
  :init
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
  (major-mode-map pandoc-mode
    :bindings
    ("pc" 'pandoc-run-pandoc
     "pof" 'pandoc-set-output-file
     "pod" 'pandoc-set-output-dir
     "pot" 'pandoc-output-format-hydra/body)
    :labels
    ("p" "pandoc"
     "po" "pandoc output")))



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
    (insert (format "[%s] " (magit-get-current-branch))))

  (add-hook 'git-commit-setup-hook 'git-commit-message-setup)

  (major-mode-map magit-mode
    :bindings
    ("" 'magit-dispatch)))

(module! git-timemachine
  :ensure t
  :defer t)

(module! forge
  :ensure t
  :after magit
  :init
  (setq forge-add-default-bindings nil))

(module! code-review
  :ensure t
  :defer t
  :init
  (setq ghub-use-workaround-for-emacs-bug 'force)
  :config
  (setq code-review-fill-column 80
	code-review-new-buffer-window-strategy #'switch-to-buffer
	code-review-download-dir "/tmp/code-review/")
  (major-mode-map code-review-mode
    :bindings
    ("m"  'code-review-transient-api
     "c" 'code-review-comment-add-or-edit)))

(module! vterm
  :ensure t
  :defer t
  :init
  (defun vterm-send-command (command &optional switch-to-vterm?)
    (let ((buf (current-buffer)))
      (when switch-to-vterm?
	(vterm))
      (vterm--goto-line -1)
      (vterm-send-string command)
      (vterm-send-return)))

  (defun vterm--get-password ()
    (let* ((base    "security find-generic-password")
	   (service "'iTerm2'")
	   (account "'1Password - iterm'")
	   (command (format "%s -s %s -a %s -w"
			    base service account)))
      (replace-regexp-in-string
       "\n" "" (shell-command-to-string command))))

  (defun vterm-send-1password ()
    (interactive)
    (save-window-excursion
      (vterm)
      (vterm--goto-line -1)
      (vterm-send-string (vterm--get-password))
      (vterm-send-return)))

  (defun vterm-send-password ()
    (let ((buf (current-buffer)))
      (vterm)
      (sleep-for 5)
      (vterm-send-string (vterm--get-password))
      (vterm-send-return)
      (vterm-clear)))

  (defun vterm-run (command)
    (save-window-excursion
      (vterm-send-command command t)))

  (defun vterm-send-paragraph ()
    (interactive)
    (let* ((start (save-excursion
		    (backward-paragraph)
		    (point)))
	   (end (save-excursion
		  (forward-paragraph)
		  (point)))
	   (command (buffer-substring start end)))
      (vterm-run command)))

;;; Figure out auto/tab complete (with lsp?)

;;;###autoload
  (define-derived-mode eirene-term-mode sh-mode "eirene-term"
    "Major mode for eirene-term files.")

  (evil-define-key  'insert eirene-term-mode-map
    (kbd "C-c C-c") 'vterm-send-paragraph
    (kbd "<tab>")   'comint-dynamic-complete-filename)
  (evil-define-key 'normal eirene-term-mode-map
    (kbd "C-c C-c") 'vterm-send-paragraph
    "mp" 'vterm-send-1password)

  (defvar *eirene-term-session* nil)
  (defvar *eirene-term-buffer* "*eirene-term*")

  (defun eirene-term ()
    (interactive)
    (if *eirene-term-session*
	(workspace-to-workspace-number *eirene-term-session* )
      (let* ((ws-num (workspace-get-next-workspace-number))
	     (ws-name (format "%s: eirene-term" ws-num)))
	(workspace--to-workspace-number ws-num ws-name)
	(let ((vterm-buffer (vterm)))
	  (split-window-right)
	  (switch-to-buffer *eirene-term-buffer*)
	  (eirene-term-mode))
	(setq *eirene-term-session* ws-num))))

  (defun eirene-term-end-session ()
    (interactive)
    (if (not *eirene-term-session*)
	(message "No eirene term session active")
      (progn
	(if (equal *eirene-term-session* 1)
	    (workspace-to-workspace-number 2)
	  (workspace-to-workspace-number 1))
	(workspace-remove-workspace-number *eirene-term-session*)
	(setq *eirene-term-session* nil)))))

(module! term
  :use-package nil
  (custom-set-faces

   '(term-color-black ((t (:foreground "#3F3F3F" :background "#2B2B2B"))))
   '(term-color-red ((t (:foreground "#AC7373" :background "#8C5353"))))
   '(term-color-green ((t (:foreground "#7F9F7F" :background "#9FC59F"))))
   '(term-color-yellow ((t (:foreground "#DFAF8F" :background "#9FC59F"))))
   '(term-color-blue ((t (:foreground "#7CB8BB" :background "#4C7073"))))
   '(term-color-magenta ((t (:foreground "#DC8CC3" :background "#CC9393"))))
   '(term-color-cyan ((t (:foreground "#93E0E3" :background "#8CD0D3"))))
   '(term-color-white ((t (:foreground "#DCDCCC" :background "#656555"))))

   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))))

(module! ag
  :defer t
  :ensure t)

(module! quelpa
  :defer t
  :ensure t)

(module! exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;; TODO: Figure out a way
;; to make module just a wrapper
;; and not use use-package.

(module! recentf-mode
  :use-package nil
  (recentf-mode))

(module! envrc
  :ensure t
  :init
  (envrc-global-mode))

(module! org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :init
  :config

  (defun end-of-buffer-p ()
    (let ((current (point))
	  (end     nil))
      (save-excursion
	(end-of-buffer)
	(setq end (point)))
      (>= current end)))

  ;; For wrapping tables in src blocks
  (defmacro org--table-enter-or-exit (exit?)
    `(let ((at-table? ,exit?))
       (while ,(if exit?
		   'at-table?
		 '(not at-table?))
	 (forward-line)
	 (when ,(if exit?
		    '(not (org-at-table-p))
		  '(or (org-at-table-p)
		       (end-of-buffer-p)))
	   (setq at-table? ,(not exit?))))))

  (defun org-to-next-table ()
    (interactive)
    (org--table-enter-or-exit nil))

  (defun org-exit-current-table ()
    (interactive)
    (org--table-enter-or-exit t))

  (defun org-src-tables ()
    (interactive)
    (while (not (end-of-buffer-p))
      (org-to-next-table)
      (let ((begin (point)))
	(org-exit-current-table)
	(let ((end (point)))
	  (goto-char begin)
	  (insert "#+BEGIN_SRC\n")
	  (goto-char end)
	  (forward-line)
	  (insert "#+END_SRC\n")))
      (forward-line)))

  (defun execute-fn-on-lines (start end buffer fn &rest args)
    (save-window-excursion
      (switch-to-buffer buffer)
      (goto-line start)
      (apply fn args)
      (dotimes (n (- end start))
	(goto-line (inc (+ start n)))
	(apply fn args))))

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

  (defun org-insert-markdown-jira-link (&optional project number)
    (interactive)
    (cl-multiple-value-bind (link-text ticket-number)
	(org-insert-link-internal project number)
      (insert (format "[%s](%s)" ticket-number link-text))))

  (defun org-insert-org-jira-link (&optional project number)
    (interactive)
    (cl-multiple-value-bind (link-text ticket-number)
	(org-insert-link-internal project number)
      (insert (format "[[%s][%s]]" link-text ticket-number))))

  (defun org-archive-finished-tasks ()
    (interactive)
    (mapcar
     (lambda (tag)
       (org-map-entries 'org-archive-subtree tag 'file))
     '("TODO=\"DONE\"" "TODO=\"WONT DO\"")))

  (defun org-insert-code-block (name language results)
    (interactive "sName: \nsLanguage: \nsResults: ")
    (insert (format "#+NAME: %s\n" name))
    ;; TODO: Make this more like a builder
    (if (equal results "")
	(insert (format "#+BEGIN_SRC %s\n\n" language))
      (insert (format
	       "#+BEGIN_SRC %s :results %s\n\n" language results)))
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

  (defun org-insert-time-stamped-row ()
    (interactive)
    (let ((time (format-time-string "%H:%M")))
      (insert (format "- %s: " time))))

  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)	; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook #'org-summary-todo)
  (add-hook 'electric-indent-functions
	    (lambda (x) (when (eq 'org-mode major-mode) 'no-indent)))

  (make-variable-buffer-local
   (defvar
     *footnote-count* 1))

  (defun org-add-footnote (text)
    (interactive "sText: ")
    (save-excursion
      (insert (format "[fn:%s]" *footnote-count*))
      (goto-char (point-max))
      (when (equal *footnote-count* 1)
	(insert "* Footnotes\n"))
      (insert "\n")
      (insert (format "[fn:%s]: %s" *footnote-count* text))
      (setq *footnote-count* (inc *footnote-count*))))

  (major-mode-map org-mode
    :bindings
    ("a"   'org-agenda
     "n"  'org-todo
     "te"  'org-set-effort
     "tp"  'org-priority
     "ts"  'org-schedule
     "tt"  'org-set-tags-command
     "ct"  'org-archive-finished-tasks
     "cs"  'org-archive-subtree
     "jo"  'org-open-at-point
     "fi"  'setup-org-file
     "fmi" 'setup-meetings-file
     "fms" 'org-meeting-insert-speaker
     "ic"  'org-insert-code-block
     "ij"  'org-insert-org-jira-link
     "ii"  'org-insert-time-stamped-row
     "if"  'org-add-footnote
     "im"  'org-insert-markdown-jira-link
     "it"  'org-jira-link-todo
     "e"   'org-export-dispatch
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
     "fm" "meeting-file"
     "f"  "file"
     "t"  "task"))

  (evil-define-key 'normal org-mode-map
    (kbd "<tab>") 'org-cycle)

  (require 'ox-md)
  (require 'ox-ipynb)
  (setq org-startup-indented t
  	org-startup-truncated nil
  	org-hide-leading-stars nil
  	org-directory "~/org"
  	org-log-done t
	org-enforce-todo-dependencies t
  	org-todo-keywords
  	'((sequence "TODO" "WORKING" "|" "DONE" "WONT DO(@)"))
  	org-hide-leading-stars t
  	org-confirm-babel-evaluate nil
  	org-agenda-files (list "~/org/status.org")
  	org-capture-default-notes-file "~/org/status.org"
	org-default-notes-file "~/org/status.org"
	org-plantuml-jar-path
	(expand-file-name
	 "/Users/andrewparisi/Documents/java/jars/plantuml.jar")
	nrepl-sync-request-timeout nil
	org-capture-templates
	'(("t" "Tasks" entry
	   (file+headline "~/org/status.org" "Tasks")
           "* TODO %?\nSCHEDULED: %^t\n")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (R . t)
     (clojure . t)
     (haskell . t)
     (emacs-lisp . t)
     (sql . t)
     (dot . t)
     (plantuml . t)
     (shell . t))))

(module! org-alert
  :ensure t
  :init
  (setq org-alert-interval 300
      org-alert-notify-cutoff 10
      org-alert-notify-after-event-cutoff 10))

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
   org-agenda-dim-blocked-tasks 'invisible
   org-agenda-overriding-columns-format
   "%TODO %7EFFORT %PRIORITY     %100ITEM 100%TAGS"
   org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t%-6e% s")
                              (todo . " %i %-12:c %-6e")
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c"))
   calendar-latitude 42.2
   calendar-longitude -71.0
   calendar-location-name "Quincy, MA"))

(module! org-timeline
  :ensure t
  :after org
  :init
  (add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/notes")
  :init
  (major-mode-map org-mode
    :labels
    ("r"  "roam"
     "rt" "roam tag")
    :bindings
    ("rf"  'org-roam-node-find
     "rl"  'org-roam-node-insert
     "rta" 'org-roam-tag-add
     "rtr" 'org-roam-tag-remove
     "rb"  'org-roam-buffer-toggle
     "ri"  'org-roam-link-current-file))
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))

  (setq
   org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new
      (file+head "${slug}.org"
                 "#+title: ${title}\n#+date: %u\n#+lastmod: \n\n")
      :immediate-finish t))
   time-stamp-start "#\\+lastmod: [\t]*")
  :config
  (org-roam-setup))

;;;;;;;
;;; LSP


(module! lsp-mode
  :ensure t
  :hook (prog-mode . display-fill-column-indicator-mode)
  :init
  (setq lsp-enable-indentation nil
	lsp-enable-completion-at-point nil
	lsp-lens-enable t
	lsp-completion-enable t
	lsp-signature-auto-activate nil)

  ;; TODO: Add these to the :hook section
  (add-hook 'lsp-mode #'lsp-enable-which-key-integration)

  ;; clojure
  (add-hook 'clojure-mode-hook #'lsp)
  (add-hook 'clojurec-mode-hook #'lsp)
  (add-hook 'clojurescript-mode-hook #'lsp)

  ;; R
  ;; (add-hook 'ess-r-mode-hook #'lsp)

  ;;yaml
  (add-hook 'yaml-mode-hook #'lsp)

  ;; python
  (add-hook 'python-mode-hook #'lsp)

  ;; scala
  (add-hook 'scala-mode-hook #'lsp)

  :config
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.flake8.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" t t)
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
		"--ignore-missing-imports"
		source-original)
      :error-patterns
      ((error line-start (file-name) ":" line ": error:" (message) line-end))
      :modes python-mode)
  (add-to-list 'flycheck-checkers 'python-mypy t)
  (flycheck-add-next-checker 'python-pylint 'python-mypy t))

(module! company
  :ensure t
  :requires evil)

(module! highlight-indent-guides
  :ensure t
  :init
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode))

            ;;;
;;;;;;;;;;;;;;;

;;;;;;;;;;
;;; python

(module! sphinx-doc
  :ensure t
  :init
  (defun get-doc-params (doc)
    (let ((fields   (sphinx-doc-doc-fields doc))
	  (results '()))
      (dolist (field fields)
	(when (equal (sphinx-doc-field-key field) "param")
	  (let* ((arg (sphinx-doc-field-arg field))
		 (doc (read-string (format "Docstring for %s: " arg))))
	    (push (format "%s: %s" arg doc) results))))
      (reverse results)))

  (defun get-doc-returns (doc)
    (let ((fields        (sphinx-doc-doc-fields doc))
	  (return-field  nil))
      (dolist (field fields)
	(when (equal (sphinx-doc-field-key field) "returns")
	  (setq return-field (sphinx-doc-field-arg field))))
      (if return-field
	  return-field
	(read-string "Returns: "))))

  (defun insert-google-comment (doc indent)
    (insert "\n")
    (let ((description (read-string "Description: "))
	  (returns     (get-doc-returns doc))
	  (params      (get-doc-params doc))
	  (indent-str  (apply 'concat (make-list indent " "))))
      (insert (format "%s\"\"\"%s\n\n" indent-str description))
      (insert (format "%sParameters\n%s----------\n"
		      indent-str indent-str))
      (dolist (param params)
	(insert (format "%s%s\n" indent-str param)))
      (insert (format "\n%sReturns\n%s-------\n"
		      indent-str indent-str))
      (insert (format "%s%s\n%s\"\"\""
		      indent-str returns indent-str))))

  (defun google-doc ()
    (interactive)
    (let* ((fn-def (sphinx-doc-str->fndef (sphinx-doc-fndef-str)))
	   (doc    (sphinx-doc-fndef->doc fn-def))
	   (indent (+ 4 (sphinx-doc-current-indent))))
      (save-excursion
	(search-forward-regexp sphinx-doc-fun-end-regex)
	(forward-line -1)
	(move-end-of-line nil)
	(insert-google-comment doc indent))))

  (defun rewrite-comment (start-line end-line)
    (interactive "nStart: \nnEnd: ")
    (let ((params '())
	  (returns nil)
	  (description nil)
	  (current-line start-line)
	  (comment-start nil)
	  (comment-end nil))
      (save-excursion
	(setq comment-start
	      (progn
		(goto-line start-line)
		(beginning-of-line)
		(point)))
	(setq comment-end
	      (progn
		(goto-line end-line)
		(end-of-line)
		(point))))
      (save-excursion
	(goto-line start-line)
	(while (<= current-line end-line)
	  (goto-line current-line)
	  (let* ((start (progn
			  (beginning-of-line)
			  (point)))
		 (end   (progn
			  (end-of-line)
			  (point)))
		 (line  (buffer-substring start end))
		 (clean (trim-whitespace line)))
	    (cond ((string-prefix-p ":param" clean)
		   (let ((param-description (car (cdr (cdr (split-string clean ":")))))
			 (param-name        (car (cdr (split-string (car (cdr (split-string clean ":"))) " ")))))
		     (push (cons param-name param-description) params)))
		  ((string-prefix-p ":returns" clean)
		   (let ((return-line (car (cdr (cdr (split-string clean ":"))))))
		     (push return-line returns)))
		  (returns
		   (push line returns))
		  (t
		   (push line description)))
	    (setq current-line (+ 1 current-line))))
	(kill-region comment-start comment-end)
	(let ((result ""))
	  (dolist (d-line (reverse description))
	    (setq result (concat result d-line "\n")))
	  (setq result (format "%s\n    Parameters\n    ----------\n" result))
	  (dolist (param (reverse params))
	    (setq result (format "%s    %s: %s\n" result (car param) (cdr param))))
	  (dolist (r-line (reverse returns))
	    (setq result (concat result r-line "\n")))
;;;	(setq result (format "%s\"\"\"" result))
	  (goto-char comment-start)
	  (insert result)))))
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
       ;;       (evil-define-key 'normal 'evil-normal-state-map (kbd "C-j") 'comint-next-input)
       ;;       (evil-define-key 'insert 'evil-insert-state-map (kbd "C-j") 'comint-next-input)
       ;;       (evil-define-key 'normal 'evil-normal-state-map (kbd "C-k") 'comint-previous-input)
       ;;       (evil-define-key 'insert 'evil-insert-state-map (kbd "C-k") 'comint-previous-input)
       ;;       (evil-define-key 'normal 'evil-normal-state-map (kbd "mc")
       ;;       'comint-clear-buffer)
;;; TODO: CLEAN THIS UP
       (evil-define-key 'normal 'inferior-python-mode-map (kbd "C-j") 'comint-next-input)
       (evil-define-key 'insert 'inferior-python-mode-map (kbd "C-j") 'comint-next-input)
       (evil-define-key 'normal 'inferior-python-mode-map (kbd "C-k") 'comint-previous-input)
       (evil-define-key 'insert 'inferior-python-mode-map (kbd "C-k") 'comint-previous-input)
       (evil-define-key 'normal 'inferior-python-mode-map (kbd "mc")
	 'comint-clear-buffer))))
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
     "e" "eval"
     "m" "move")
    :bindings
    ("dr" 'rewrite-comment
     "ds" 'sphinx-doc
     "dg" 'google-doc
     "b"  'blacken-buffer
     "g"  'lsp-find-definition
     "mn" 'python-nav-forward-defun
     "mp" 'python-nav-backward-defun
     "eb" 'python-shell-send-buffer
     "ed" 'python-shell-send-defun))

  (evil-define-key 'visual python-mode-map "m;" 'comment-or-uncomment-region))


(module! blacken
  :ensure t
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq blacken-line-length '79))


(module! pyvenv
  :ensure t
  :defer t
  :init
  (setenv "CONDA_PREFIX" "/Users/andrewparisi/opt/anaconda3")
  (setenv "WORKON_HOME" (concat (getenv "CONDA_PREFIX") "/envs"))
  :config
  (pyvenv-mode t)

  ;; Set correct Python interpreter
;;  (setq pyvenv-post-activate-hooks
;;        (list (lambda ()
;;                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
;;  (setq pyvenv-post-deactivate-hooks
;;        (list (lambda ()
;;                (setq python-shell-interpreter "python3"))))


  )


(module! ein
  :defer t
  :ensure t
  :init (setq ein:output-area-inlined-images t))

                 ;;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;;; emacs lisp

(module! emacs
  :use-package nil
  (major-mode-map emacs-lisp-mode
    :labels
    ("e" "eval")
    :bindings
    ("eb" 'eval-buffer
     "ed" 'eval-defun
     "ee" 'eval-last-sexp
     "ep" 'pp-eval-last-sexp
     "g"  'xref-find-definitions
     "."  'xref-prompt-find-definitions
     ","  'xref-pop-marker-stack
     "t"  'trace-function
     "u"  'untrace-function)))


;;;;;;;;;;;
;;; clojure

(module! cider
  :ensure t
  :defer t
  :mode ("\\.clj\\'" . clojure-mode)
  :requires evil
  :init
  (setq tab-always-indent 'complete)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
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

  (defun xref-prompt-find-definitions ()
    (interactive)
    (let* ((backend (xref-find-backend))
           (completion-ignore-case
            (xref-backend-identifier-completion-ignore-case backend))
	   (id
            (completing-read
	     "Find Definitions: "
	     (xref-backend-identifier-completion-table backend)
             nil nil nil
             'xref--read-identifier-history)))
      (if (equal id "")
          (user-error "There is no default identifier")
	(xref--find-definitions id nil))))

  (major-mode-map clojure-mode
    :bindings
    ("jj" 'my-cider-jack-in
     "jc" 'my-cider-connect
     "jq" 'cider-quit
     "el" 'cider-load-buffer
     "ee" 'cider-eval-defun-at-point
     "ec" 'cider-eval-defun-to-comment
     "ep" 'cider-pprint-eval-defun-at-point
     "fd" 'cider-format-defun
     "fb" 'cider-format-buffer
     "q"  'cider-quit
     "s"  'cider-toggle-trace-var
     "n"  'cider-repl-set-ns
     "g"  'xref-prompt-find-definitions
     "."  'xref-find-definitions
     ","  'xref-pop-marker-stack
     "c"  'cider-eval-defun-at-point
     "r"  'lsp-find-references
     "d"  'lsp-describe-thing-at-point
     "tn" 'cider-test-run-ns-tests
     "tp" 'cider-test-run-project-tests
     "tt" 'cider-test-run-test
     "a"  'lsp-execute-code-action)
    :labels
    (""  "major mode"
     "e" "eval"
     "f" "format"
     "t" "test"
     "l" "cider load"
     "j"  "repl"))
  :config
  (setq lsp-clojure-server-command '("clojure-lsp")
	    org-babel-clojure-backend 'cider))

           ;;;
;;;;;;;;;;;;;;

;;;;;;;;;
;;; scala

(module! scala-mode
  :ensure t
  :defer t
  :mode "\\.s\\(cala\\|bt\\)$"
  :config
  (major-mode-map scala-mode
    :labels
    ("s" "sbt")
    :bindings
    ("a" 'lsp-execute-code-action
     "sc" 'sbt-do-run
     "st" 'sbt-do-test
     "ss" 'sbt-start
     "n" 'lsp-metals-new-scala-file
     "g" 'lsp-find-definition
     "r" 'lsp-find-definiions
     "d" 'lsp-describe-thing-at-point))
  )

(module! sbt-mode
  :ensure t
  :defer t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(module! lsp-metals
  :ensure t
  :defer t)

         ;;;
;;;;;;;;;;;;


;;;;;;;;;;;
;;; haskell

(module! haskell-mode
  :ensure t
  :defer t
  :requires (evil which-key)

  :mode ("\\.hs\\'" . haskell-mode)
  :config
  (require 'ob-haskell))

            ;;;
;;;;;;;;;;;;;;;

;;;;;
;;; R

 (module! ess
   ;; This doesn't really work in a terminal
   :ensure t
   :defer t)



            ;;;
;;;;;;;;;;;;;;;


;;;;;;;;
;;; mail

;;(module! mu4e
;;  :load-path "/usr/local/share/emacs/site-lisp/mu@1.6.6/mu4e/"
;;  :config
;;  (setq mu4e-mu-binary (executable-find "mu")
;;	mu4e-maildir "~/.maildir"
;;	mu4e-drafts-folder "/Users/andrewparisi/.maildir/drafts"
;;	mu4e-sent-folder   "/Users/andrewparisi/.maildir/sent"
;;	mu4e-trash-folder  "/Users/andrewparisi/.maildir/trash"
;;	mu4e-get-mail-command (concat
;;			       (executable-find "mbsync")
;;			       " -a")
;;	mu4e-update-interval 300
;;	mu4e-attachment-dir "~/Desktop"
;;	mu4e-change-filename-when-moving t
;;	mu4e-user-mail-address-list '("andrew.parisi@reifyhealth.com")
;;	mu4e-confirm-quit nil)
;;  ;; sending emails
;;  (require 'smtpmail)
;;  ;;(require 'epa-file)
;;  ;;(epa-file-enable)
;;  (auth-source-forget-all-cached)
;;  (setq epa-pinentry-mode 'loopback
;;	message-kill-buffer-on-exit t
;;	send-mail-function 'sendmail-send-it
;;	message-send-mail-function 'sendmail-send-it
;;	sendmail-program (executable-find "msmtp"))
;;
;;  (defun timu/set-msmtp-account ()
;;    (if (message-mail-p)
;;	(save-excursion
;;          (let*
;;              ((from (save-restriction
;;                       (message-narrow-to-headers)
;;                       (message-fetch-field "from")))
;;               (account "gmail"))
;;            (setq message-sendmail-extra-arguments (list '"-a" account))))))
;;  (add-hook 'message-send-mail-hook 'timu/set-msmtp-account)
;;
;;  ;; mu4e cc & bcc
;;  ;; this is custom as well
;;  (add-hook 'mu4e-compose-mode-hook
;;            (defun timu/add-cc-and-bcc ()
;;              "My Function to automatically add Cc & Bcc: headers.
;;    This is in the mu4e compose mode." (save-excursion (message-add-header "Cc:\n"))
;;              (save-excursion (message-add-header "Bcc:\n"))))
;;
;;  ;; mu4e address completion
;;  (add-hook 'mu4e-compose-mode-hook 'company-mode)
;;
;;  ;; For some reason this is throwing an error
;; ;; (major-mode-map mu4e-view-mode
;; ;;   :bindings
;; ;;   ("c" 'mu4e-org-store-and-capture))
;;  )

             ;;;;
;;;;;;;;;;;;;;;;;

;;;;;;;
;;; SQL

(module! sqlformat
  :ensure t
  :defer t
  :config
  (setq sqlformat-command 'pgformatter
	sqlformat-args '("-s2" "-g")))

(module! sql
  :defer t
  :init

  (defun sql-add-newline-first (output)
    "Add newline to beginning of OUTPUT for `comint-preoutput-filter-functions'"
    (if (equal major-mode 'sql-interactive-mode)
	(concat "\n" output)
      output))

  (defun sqli-add-hooks ()
    "Add hooks to `sql-interactive-mode-hook'."
    (add-hook 'comint-preoutput-filter-functions
              'sql-add-newline-first))

  (defun sql-get-password (key account)
    (let ((command (concat  "security "
			    "find-generic-password "
			    "-s '"
			    key
			    "' -a '"
			    account
			    "' -w")))
      (->> command shell-command-to-string split-string car)))

  (defun sql-get-aws-password (environment database role)
    (string-trim
     (shell-command-to-string
      (format "rh-tools db url %s %s %s" environment database role))))

  (setq sql-postgres-login-params nil
	sql-connection-alist
	'((psql-prod-concept-data
	   (sql-product 'postgres)
	   (sql-database
	    (sql-get-aws-password
	     "production" "concept-data-production" "production-db")))
          (psql-prod-development
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
          (psql-testing-concept-data
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
	  (psql-kb
	   (sql-product 'postgres)
	   (sql-database
	    "postgresql://localhost/kb"))
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
	  (local-concept-data
	   (sql-product 'postgres)
	   (sql-database
	    "postgresql://localhost/concept_data"))
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
  (add-hook 'sql-interactive-mode-hook 'sqli-add-hooks)
  (add-hook 'sql-interactive-mode-hook
	    (lambda () (toggle-truncate-lines t)))

  (evil-define-key 'insert sql-mode-map (kbd "C-c p") 'autocomplete-table)
  (evil-define-key 'normal sql-mode-map (kbd "C-c p") 'autocomplete-table))


(module! restclient
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))


(module! tex
  :use-package nil
  (defun tex-remove-tag-at-point ()
    (interactive)
    (save-excursion
      (re-search-backward (regexp-quote "\\"))
      (let ((start (point)))
	(re-search-forward (regexp-quote "{"))
	(let ((brace-start (point)))
	  (backward-char)
	  (evil-jump-item)
	  (let ((brace-end (point)))
	    (message (format "POINTS: %s %s %s" start brace-start brace-end))
	    (kill-region brace-end (+ 1 brace-end))
	    (kill-region start brace-start))))))

  (major-mode-map latex-mode
    :labels ("t" "tag")
    :bindings ("tx" 'tex-remove-tag-at-point))

  (defvar tex-compile-commands
    `(,@(mapcar (lambda (prefix)
                  `((concat ,prefix tex-command
                            " " tex-start-options
                            " " (if (< 0 (length tex-start-commands))
                                    (shell-quote-argument tex-start-commands))
                            " %f")
                    t "%r.pdf"))
		'("pdf" "xe" "lua"))
      ((concat tex-command
	       " " (if (< 0 (length tex-start-commands))
		       (shell-quote-argument tex-start-commands))
               " %f")
       t "%r.dvi")
      ("xdvi %r &" "%r.dvi")
      ("\\doc-view \"%r.pdf\"" "%r.pdf")
      ("open %r.pdf" "%r.pdf")
      ("xpdf %r.pdf &" "%r.pdf")
      ("gv %r.ps &" "%r.ps")
      ("yap %r &" "%r.dvi")
      ("advi %r &" "%r.dvi")
      ("gv %r.pdf &" "%r.pdf")
      ("bibtex %r" "%r.aux" "%r.bbl")
      ("makeindex %r" "%r.idx" "%r.ind")
      ("texindex %r.??")
      ("dvipdfm %r" "%r.dvi" "%r.pdf")
      ("dvipdf %r" "%r.dvi" "%r.pdf")
      ("dvips -o %r.ps %r" "%r.dvi" "%r.ps")
      ("ps2pdf %r.ps" "%r.ps" "%r.pdf")
      ("lpr %r.ps" "%r.ps"))
    "List of commands for `tex-compile'.
Each element should be of the form (FORMAT IN OUT) where
FORMAT is an expression that evaluates to a string that can contain
  - `%r' the main file name without extension.
  - `%f' the main file name.
IN can be either a string (with the same % escapes in it) indicating
  the name of the input file, or t to indicate that the input is all
  the TeX files of the document, or nil if we don't know.
OUT describes the output file and is either a %-escaped string
  or nil to indicate that there is no output file.")
  )


           ;;;
;;;;;;;;;;;;;;

;;;;;;;
;;; CSV

(module! csv-mode
  :ensure t
  :defer t
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

(module! terraform-mode
  :ensure t
  :defer t
  :config
  (major-mode-map terraform-mode
    (:bindings
     "f" 'terraform-format-buffer))
  )



       ;;;
;;;;;;;;;;

;;; Docker

(module! docker
  :use-package nil
  :load "/Users/andrewparisi/emacs-files/docker")

(module! dockerfile-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-mode)))

       ;;;
;;;;;;;;;;

;;;;;;;;;;;;;
;;; Utilities

(module! yaml-mode
  :defer t
  :ensure t)

(module! crdt
  :defer t
  :ensure t)


;;;;;;;;;;;;;
;;; Semantics

(module! ttl-mode
  :defer t
  :ensure t)

(module! sparql-mode
  :defer t
  :ensure t)

;;;
;;;;;;;;;;;;;;

(module! poetry
  :ensure t
  :defer t
  :init
  (add-to-list 'exec-path "/Users/andrewparisi/.poetry/bin")

  (defun poetry-pytest ()
    (interactive)
    (poetry-run "pytest"))

  (major-mode-map python-mode
    :labels
    ("t" "test")
    :bindings
    ("tt" 'poetry-pytest)))

;;;;;;;;;;
;; Scratch


;;; (vc-git--rev-parse "HEAD")
;;;
;;;
;; (let ((version nil))
;;   (save-window-excursion
;;     (switch-to-buffer "rds.tf")
;;     (setq version (vc-git--rev-parse "HEAD")))
;;   (insert version))


(module! typescript-mode
  :ensure t
  :defer t
  :init
  (defvar *npm-term* "*npm-term*")
  (defvar *npm-term-started?* nil)

  (defun npm-start ()
    (interactive)
    (if (not *npm-term-started?*)
	(save-window-excursion
	  (let ((current-dir default-directory)
		(buffer      (vterm *npm-term*)))
	    (switch-to-buffer buffer)
	    (vterm-send-string (format "cd %s" current-dir))
	    (vterm-send-return)
	    (vterm-send-string "npm start")
	    (vterm-send-return)
	    (setq *npm-term-started?* t)))
      (message "npm term has already been started")))

  (defun npm-check-compile ()
    (interactive)
    (let ((return-to-compile? nil)
	  (prompt "Press 'y' to remain here, any other key to return"))
      (if *npm-term-started?*
	  (progn
	    (save-window-excursion
	      (switch-to-buffer *npm-term*)
	      (let ((keypress (read-char prompt)))
		(when (equal keypress ?y)
		  (setq return-to-compile? t))))
	    (when return-to-compile?
	      (switch-to-buffer *npm-term*)
	      (vterm--goto-line -1)))
	;; TODO this should prompt user to start if they want
	(message "npm has not been started"))))

  (defun npm-quit ()
    (interactive)
    (if *npm-term-started?*
	(save-window-excursion
	  (switch-to-buffer *npm-term*)
	  (vterm-send-C-c)
	  (vterm-send-C-c)
	  (setq *npm-term-started?* nil)
	  (kill-buffer *npm-term*))
      (message "npm has not been started")))

  (defun ts-new-file ()
    (interactive)
    (let ((project-path-len (length tide-project-root)))
      (let ((full-path (counsel-find-file)))
	(save-buffer)
	(let ((relpath (substring full-path project-path-len)))
	  (goto-char 0)
	  (insert (format "// %s\n" relpath))))))

  (major-mode-map typescript-mode
    :bindings
    ("s" 'npm-start
     "c" 'npm-check-compile
     "q" 'npm-quit
     "f" 'ts-new-file)))


(module! tide
  :ensure t
  :defer t
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

  ;; if you use typescript-mode
  (add-hook 'typescript-mode-hook #'setup-tide-mode)

  ;; if you use treesitter based typescript-ts-mode (emacs 29+)
  ;; (add-hook 'typescript-ts-mode-hook #'setup-tide-mode)
  )
