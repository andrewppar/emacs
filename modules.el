(load! "~/.emacs.d/keyboard.el")


;;TODO: Are these necessary?
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/Users/anparisi/go/bin")


;; TODO: Create module! macro that is a thin wrapper around use-package
;; but that makes defining major mode and default bindings for a mode easy
;; TODO: Figure a way to auto configure lsp mode for a language
;; TODO: Add native quelpa support and ensure we can call it from
;; module!
;;;;;;;;;;;;;;;;;;;;
;; Required Packages
;; TODO See whether or not these can be paired down

(defvar *status-file* "/Users/anparisi/org/status.org")

(module! undo-tree
  :ensure t
  :requires evil
  :diminish
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (evil-set-undo-system 'undo-tree))

(module! tmuxmacs
  :straight (tmuxmacs :host github :repo "andrewppar/tmuxmacs")
  :ensure t
  :defer t
  :init
  (defun send-window-to-background ()
    (interactive)
    (let ((window (tmux--get-window)))
      (tmux-window/to-session window (tmux-session/find-or-make "background"))))

  (defun send-window-to-main ()
    (interactive)
    (let ((window (tmux--get-window))
	  (main-session nil))
      (when-let ((session (tmux-session/find "main")))
	(setq main-session session))
      (unless main-session
	(if-let ((focused-session (tmux-session/focused)))
	    (progn
	      (tmux-session/rename focused-session "main")
	      (setq main-session focused-session))
	  (setq main-session (tmux-session/make "main"))))
      (tmux-window/to-session window main-session)))

  (defun get-bash-history ()
    (let ((buffer-result nil)
	  (result nil))
      (save-window-excursion
	(let ((buffer (find-file (expand-file-name "~/.zsh_history"))))
	  (switch-to-buffer buffer)
	  (setq buffer-result (buffer-substring (point-min) (point-max)))
	  (kill-buffer buffer)))
      (dolist (command (split-string buffer-result "\n"))
	(unless (member command result)
	  (push command result)))
      result))

  (defun new-window-with-command ()
    (interactive)
    (let* ((history (get-bash-history))
	   (command (ivy-completing-read "command: " history))
	   (window nil)
	   (pane nil))
      (save-tmux-excursion
	(setq window (tmux-window/make nil (tmux-session/focused)))
	(setq pane (car (tmux-pane/list window))))
      (tmux-pane/send-command pane command)))

  :config
  (transient-append-suffix 'tmuxmacs  "w"
    '("b" "send to background" send-window-to-background))
  (transient-append-suffix 'tmuxmacs  "w"
    '("m" "send to main" send-window-to-main))
  (transient-append-suffix 'tmuxmacs "w"
    '("c" "new window with command" new-window-with-command))
  )



(module! timesheet
  :use-package nil
  :init (add-to-list
	 'load-path
	 (expand-file-name "~/.emacs.d/timesheet")))


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

(module! sexpressions
  :use-package nil
  (defun drag-sexp-backwards (&optional times)
    "Drag an s-expression bacwards.

   Optionally, move it TIMES backward."
    (interactive)
    (let ((times (or times 1)))
      (dotimes (_i times)
	(transpose-sexps 1)
	(backward-sexp 2))))

  (defun drag-sexp-forwards (&optional times)
    "Drag an s-expression forwards.

   Optionally, move it TIMES forwards."
    (interactive)
    (let ((times (or times 1)))
      (dotimes (_i times)
	(forward-sexp)
	(transpose-sexps 1)
	(backward-sexp))))

  (defun drag-binding-backwards ()
    "Drag two sexpressions backward."
    (interactive)
    (drag-sexp-backwards)
    (drag-sexp-backwards)
    (forward-sexp)
    (forward-sexp)
    (forward-sexp)
    (drag-sexp-backwards)
    (drag-sexp-backwards)))

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

      ("clojure"
       (or (mode . clojure-mode)
	   (directory . "/Users/anparisi/Documents/clojure")
	   (name . "\*cider\*")))
      ("magit"
       (name . "*magit*"))
      ("help"
       (or (name . "\*Help\*")
	   (name . "\*Apropos\*")
	   (name . "\*info\*")))
      ("keep"
       (or (name . "*Org Agenda*")
	   (name . "*todays-task-log*")
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

(module! diranger
  :straight (diranger :host github :repo "andrewppar/diranger")
  :ensure t
  :init
  (evil-define-key 'normal diranger-mode-map
  "C" #'dirnager-copy
  "D" #'diranger-delete
  "f" #'diranger-jump
  "gr" #'diranger-refresh
  "h" #'diranger-out
  "j" #'diranger-forward-line
  "k" #'diranger-backward-line
  "l" #'diranger-into
  "mx" #'diranger-mark-delete
  "mu" #'diranger-unset-mark-at-point
  "mU" #'diranger-unset-all-marks
  "pp" #'diranger-paste
  "pr" #'diranger-paste-from-ring
  "q" #'diranger-quit
  "R" #'diranger-rename
  "x" #'diranger-execute-marks
  "yy" #'diranger-yank
  "yx" #'diranger-cut
  (kbd "RET") #'diranger-into))

(module! treemacs
  :use-package nil
  :init
  (evil-define-key
    'normal treemacs-mode-map
    "l" 'treemacs-TAB-action
    "+" 'treemacs-create-dir
    "f" 'treemacs-create-file
    ))

(module! magit
  :ensure t
  :defer t
  :config
  (load "/Users/anparisi/emacs-files/github.el")
  (load "/Users/anparisi/emacs-files/pair.el")
  (setq
   magit-display-buffer-function
   #'magit-display-buffer-fullframe-status-v1
   ediff-window-setup-function
   #'ediff-setup-windows-plain)

  (defun git-commit-message-setup ()
    (insert (format "[%s] " (magit-get-current-branch)))
    (insert "\n\n")
    (comment
     (when (string-match
	    (regexp-quote "projects/conure") default-directory)
       (insert (with-temp-buffer
		 (insert-file-contents
		  "/Users/anparisi/.emacs.d/conure-commit.txt")
		 (buffer-string))))))

  ;; TODO: Make these dynamically created in the project file?
  (defclass repo ()
    ((name         :initarg :name
		   :type string
		   :documentation  "The name of the repo")
     (test-command :initarg :test-command
		   :type string
		   :documentation "The test command for the repo"))
    "A class for github repos")

  (defun git-commit-run-tests ())

  (add-hook 'git-commit-setup-hook 'git-commit-message-setup)


  (defun git-commit-check-tasks ()
    (let* ((branch       nil)	   ;; (magit-get-current-branch))
	   (ticket       nil)	   ;;(car (split-string branch "/")))
	   (ticket-header  "GH-228") ;;(upcase ticket))
	   (tasks         '()))
      (save-window-excursion
	(find-file *status-file*)
	(let ((section-start (org-header-position
			      `("Tasks" "Engine Team" ,ticket-header)))
	      (section-end nil))
	  (goto-char section-start)
	  (let* ((header (thing-at-point 'line))
		 (depth  (org-header-depth header)))
	    (while (not section-end)
	      (let ((forward? (forward-line 1))
		    (line (thing-at-point 'line)))
		(when (or (equal forward? 1)
			  (and (org-header-p line)
			       (equal (org-header-depth line) depth)))
		  (setq section-end (point)))))
	    (goto-char section-start)
	    (while (< (point) section-end)
	      (forward-line 1)
	      (let ((line (thing-at-point 'line)))
		(when (and (org-header-p line)
			   (> (org-header-depth line) depth)
			   (org-header-not-done-p line))
		  (push (org-header-task line) tasks)))))))
      tasks))

  ;; TODO: This should be doable with major-mode-map
  (evil-define-key 'normal magit-mode-map
    "p" 'pair-dispatch
    "G" 'gh-dispatch)

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

(module! rg
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

(module! recentf-mode
  :use-package nil
  (recentf-mode))

(module! envrc
  :ensure t
  :init
  (envrc-global-mode))

(module! just-mode
  :ensure t
  :defer t)

(module! markdown
  :use-package nil
  (major-mode-map markdown-mode
    :bindings
    ("l" 'org-toggle-latex-fragment
     "u" 'markdown-toggle-url-hiding
     "p" 'org-latex-preview)))

(module! org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :init
  :config

  (defun execute-fn-on-lines (start end buffer fn &rest args)
    (save-window-excursion
      (switch-to-buffer buffer)
      (goto-line start)
      (apply fn args)
      (dotimes (n (- end start))
	(goto-line (inc (+ start n)))
	(apply fn args))))

;;  (defmacro do-file (header-var &rest body)
;;    (declare (indent 1))
;;    (let ((pos (gensym)))
;;      `(save-excursion
;;	 (goto-char 0)
;;	 (setq
;;
;;      )

  (defun header-text (string)
    "Get the text from a STRING thats an org mode header."
    (-> string
	string-trim
	split-string
	cdr
	(string-join " ")
	string-trim))

  (defmacro do-org-headers (header-var &rest body)
    (declare (indent 1))
    (let ((pos    (gensym)))
      `(save-excursion
	 (goto-char 0)
	 (cl-do ((,pos (re-search-forward "^\*" nil t)
		       (re-search-forward "^\*" nil t)))
	     ((not ,pos) nil)
	   (setq ,header-var (header-text (thing-at-point 'line)))
	   ,@body))))

  (defun org-header-not-done-p (header))

  (defun org-header-task (header))

  (defun org-header-depth (header)
    (-> header string-trim split-string car length))

  (defun org-header-p (line)
    (string-match "^\*" line))

  (defun org-header-position (header-list)
    (let ((pos nil)
	  (hl  header-list))
      (do-org-headers header
	(when (equal header (car hl))
	  (when (not (cdr hl))
	    (setq pos (point)))
	  (setq hl (cdr hl))))
      pos))

  (defun org-import-to-status-file ()
    (interactive)
    (when (org-entry-is-todo-p)
      (save-window-excursion
	(find-file *status-file*)
	(save-excursion
	  (goto-char (org-header-position '("Tasks" "Import")))
	  (forward-line)
	  (let* ((entry (assoc-delete-all
			 "BLOCKED" (org-entry-properties)))
		 (item  (alist-get "ITEM" entry nil t #'equal))
		 (todo  (alist-get "TODO" entry nil t #'equal))
		 (priority (alist-get "PRIORITY" entry nil t #'equal))
		 (origin   (alist-get "FILE" entry nil t #'equal))
		 (tags     (alist-get "ALLTAGS" entry nil t #'equal))
		 (properties ":PROPERTIES:\n")
		 (result     ""))
	    (setq properties (format "%s :ORIGIN: %s\n" properties origin))
	    (dolist (binding entry)
	      (cl-destructuring-bind (property . value)
		  binding
	      (setq properties
		    (format "%s:%s: %s\n" properties property value))))
	    (setq properties
		  (format "%s:END:\n" properties))
	    (setq result (format "*** %s " todo))
	    (when priority
	      (setq result (format "%s [#%s]" result priority)))
	    (setq result (format "%s %s" result item))
	    (when tags
	      (setq result (format "%s %s" result tags)))
	    (setq result (format "%s\n%s\n" result properties))
	    (insert result))))))

  (defun org-insert-github-issue-link (number)
    (interactive "nIssue Number: ")
    (insert
     (format
      "[[https://github.com/advthreat/conure/issues/%s][#%s]]"
      number number)))

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
      (goto-char 0)
      (insert (format "#+title: %s\n" title))
      (insert (format "#+date: %s\n" date))
      (insert (format "#+author: Andrew Parisi\n"))))

  (defun org-insert-time-stamped-row ()
    (interactive)
    (let ((time (format-time-string "%H:%M")))
      (insert (format "- %s | " time))))

  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)	; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook #'org-summary-todo)
  (add-hook 'electric-indent-functions
	    (lambda (x) (when (eq 'org-mode major-mode) 'no-indent)))
  (add-hook 'org-mode-hook 'flyspell-mode)

  (defun org-code (value)
    (interactive "sText: ")
    (insert (format "=%s=" value)))

  (major-mode-map org-mode
    :bindings
    ("a"   'org-agenda
     "d"   'org-ctrl-c-ctrl-c
     "n"   'org-todo
     "te"  'org-set-effort
     "tp"  'org-priority
     "ts"  'org-schedule
     "tt"  'org-set-tags-command
     "ct"  'org-archive-finished-tasks
     "cs"  'org-archive-subtree
     "jo"  'org-open-at-point
     "fi"  'setup-org-file
     "hp"  'org-set-property
     "ib"  'org-insert-code-block
     "ic"  'org-code
     "ii"  'org-insert-github-issue-link
     "it"  'org-insert-time-stamped-row
     "e"   'org-export-dispatch
     "p"   'org-generate-pr-url
     "mp"  'org-move-subtree-up
     "mn"  'org-move-subtree-down
     "mj"  'org-move-item-down
     "mk"  'org-move-item-up
     "mh"  'org-promote-subtree
     "ml"  'org-demote-subtree
     "si" 'org-toggle-inline-images
     ""  'org-meta-return)
    :labels
    ("i"  "insert"
     "j"  "jump"
     "m"  "move"
     "h"  "header"
     "s"  "settings"
     "c"  "clear"
     "f"  "file"
     "t"  "task"))

  (evil-define-key 'normal org-mode-map
    (kbd "<tab>") 'org-cycle)

;;; Capture

  ;; Do something a little more flexible with this
  (defmacro capture-entry (key header)
    `'(,key ,header entry
	    (file+headline ,*status-file* ,header)
       "*** TODO %?\nSCHEDULED: %^t\n"))

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
  	org-agenda-files (list *status-file*)
  	org-capture-default-notes-file *status-file*
	org-default-notes-file *status-file*
	nrepl-sync-request-timeout nil
	org-capture-templates
	(list
	 (capture-entry "p" "Pipes")
	 (capture-entry "s" "Spacephoenix")
	 (capture-entry "l" "Learning")
	 (capture-entry "k" "Scotus")
	 (capture-entry "e" "Emacs")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (emacs-lisp . t)
     (sql . t)
     (dot . t)
     (plantuml . t)
     (shell . t))))

(module! org-agenda
  :use-package nil
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (evil-define-key 'normal org-agenda-mode-map
    "b" 'org-agenda-earlier
    "c" 'org-capture
    "d" 'org-agenda-day-view
    "D" 'org-agenda-goto-date
    "e" 'org-agenda-set-effort
    "f" 'org-agenda-later
    "p" 'org-agenda-priority
    "q" 'org-agenda-quit
    "r" 'org-agenda-redo
    "s" 'org-agenda-date-prompt
    "t" 'org-agenda-todo
    "w" 'org-agenda-week-view
    "." 'org-agenda-goto-today
    (kbd "<RET>") 'org-agenda-goto)
  (setq
   org-agenda-dim-blocked-tasks t
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
  (setq org-timeline-prepend nil)

  (defun org-color-holidays-green ()
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "holiday:" nil t)
	(add-text-properties (match-beginning 0) (point-at-eol)
			     '(face (:foreground "#AFD75F"))))))

  (add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append)
  (add-hook 'org-agenda-finalize-hook 'org-color-holidays-green :append))

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
	lsp-enable-snipped nil
	lsp-signature-auto-activate nil
	lsp-headerline-breadcrumb-mode nil
	lsp-headerline-breadcrumb-enable nil)
;;  (defface lsp-headerline-breadcrumb-path-face '((t :inherit font-lock-function-face))
;;    "Face used for breadcrumb paths on headerline."
;;    :group 'lsp-headerline)
;;
;;  (defface lsp-headerline-breadcrumb-symbols-face
;;      '((t :inherit font-lock-function-face :weight bold))
;;    "Face used for breadcrumb symbols text on headerline."
;;    :group 'lsp-headerline)

  ;; sql
  (add-hook 'sql-mode-hook 'lsp)
  (setq lsp-sqls-workspace-config-path nil)

  ;; TODO: Add these to the :hook section
  (add-hook 'lsp-mode #'lsp-enable-which-key-integration)

  ;; clojure
  (add-hook 'clojure-mode-hook #'lsp)
  (add-hook 'clojurec-mode-hook #'lsp)
  (add-hook 'clojurescript-mode-hook #'lsp))

;; lsp-treemacs

;;(module! lsp-ui
;;  :ensure t
;;  :after (lsp-mode)
;;  :init (setq lsp-ui-doc-enable t
;;              lsp-ui-doc-use-webkit t
;;              lsp-ui-doc-header t
;;              lsp-ui-doc-delay 0.2
;;              lsp-ui-doc-include-signature t
;;              lsp-ui-doc-alignment 'at-point
;;              lsp-ui-doc-use-childframe t
;;              lsp-ui-doc-border (face-foreground 'default)
;;              lsp-ui-peek-enable t
;;              lsp-ui-peek-show-directory t
;;	      lsp-ui-sideline-show-diagnostics t
;;              lsp-ui-sideline-enable t
;;              lsp-ui-sideline-show-code-actions t
;;              lsp-ui-sideline-show-hover t
;;              lsp-ui-sideline-ignore-duplicate t)
;;  :config
;;  (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))
;;
;;  ;; `C-g'to close doc
;;  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
;;
;;  ;; Reset `lsp-ui-doc-background' after loading theme
;;  (add-hook 'after-load-theme-hook
;;	    (lambda ()
;;              (setq lsp-ui-doc-border (face-foreground 'default))
;;              (set-face-background 'lsp-ui-doc-background
;;				   (face-background 'tooltip))))
;;
;;  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
;;  ;; @see https://github.com/emacs-lsp/lsp-ui/issues/243
;;  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
;;    (setq mode-line-format nil)))

(module! flycheck
  :ensure t
  :defer t
  :init
  (global-flycheck-mode))

(module! company
  :ensure t
  :requires evil)

            ;;;
;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;;; emacs lisp

(module! emacs
  :use-package nil
  (defun elisp-debug-function ()
    (interactive)
    (eval-defun t))
  (evil-define-key 'normal emacs-lisp-mode-map
    (kbd "L") 'forward-sexp
    (kbd "H") 'backward-sexp)
  (major-mode-map emacs-lisp-mode
    :labels
    ("a"
     "action"
     "e" "eval")
    :bindings
    ("af" 'clojure-thread-first-all
     "al" 'clojure-thread-last-all
     "ad" 'elisp-debug-function
     "eb" 'eval-buffer
     "ed" 'eval-defun
     "ee" 'eval-last-sexp
     "ep" 'pp-eval-last-sexp
     "j" 'forward-sexp
     "k" 'backward-sexp
     "q" 'indent-pp-sexp
     "g"  'xref-find-definitions
     "m"  'emacs-lisp-macroexpand
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
	  nrepl-use-ssh-fallback-for-remote-hosts t
	  cider-show-eval-spinner t
	  clojure-toplevel-inside-comment-form t
	  cider-repl-display-help-banner nil
	  cider-show-error-buffer t))

(module! clojure-mode
  :ensure t
  :defer t
  :mode ("\\.clj\\'" . clojure-mode)
  :requires (evil which-key)
  :init
  (defun my-cider-op (op &rest args)
    (apply op args)
    (major-mode-map cider-repl-mode
	:bindings
      ("c" 'cider-repl-clear-buffer
	   "k" 'cider-repl-previous-input
	   "j" 'cider-repl-next-input)
      :labels
      ("" "major mode")))

  (defun my-cider-jack-in ()
    (interactive)
    (my-cider-op 'cider-jack-in '()))

  (defun my-cider-connect ()
    (interactive)
    (my-cider-op 'cider-connect-clj '()))

  (defun xref-prompt-find-definitions ()
    (interactive)
    (let* ((backend (xref-find-backend))
           (completion-ignore-case
            (xref-backend-identifier-completion-ignore-case backend))
	   (id
            (completing-read
	     "Find Definitions: "
	     (xref-backend-identifier-completion-table backend)
             nil t (thing-at-point 'word 'no-properties)
             'xref--read-identifier-history)))
      (if (equal id "")
          (user-error "There is no default identifier")
	(xref--find-definitions id nil))))

  (defun clojure-toggle-debug-at-point ()
    (interactive)
    (let ((debugged? nil))
      (save-excursion
	(forward-line -1)
	(setq debugged? (->> (thing-at-point 'line)
			     string-trim
			     (string-prefix-p "#dbg"))))
      (if debugged?
	  (progn
	    (forward-line -1)
	    (delete-line)
	    (forward-line 1))
	(progn
	  (evil-open-above 1)
	  (insert "#dbg")))
      (cider-eval-defun-at-point)
      (evil-normal-state)))

  (defun cider-turn-on-debug-at-point ()
    (interactive)
    (cider-eval-defun-at-point t))

  (major-mode-map clojure-mode
      :bindings
    ("."  'xref-find-definitions
	  ","  'xref-pop-marker-stack
	  "'" 'clojure-toggle-ignore
	  "aa" 'lsp-execute-code-action
	  "ad" 'cider-turn-on-debug-at-point
	  "ae" 'cider-englighted-mode
	  "af" 'clojure-thread-first-all
	  "al" 'clojure-thread-last-all
	  "an" 'clojure-update-ns
	  "as" 'cider-toggle-trace-var
	  "au" 'clojure-unwind-all
	  "dd" 'cider-doc
	  "de" 'eldoc-doc-buffer
	  "eb" 'cider-load-buffer
	  "ee" 'cider-pprint-eval-last-sexp
	  "ed" 'cider-eval-defun-at-point
	  "ec" 'cider-eval-defun-to-comment
	  "ep" 'cider-pprint-eval-defun-at-point
	  "fd" 'cider-format-defun
	  "fb" 'cider-format-buffer
	  "g"  'xref-prompt-find-definitions
	  "jj" 'my-cider-jack-in
	  "jc" 'my-cider-connect
	  "jq" 'cider-quit
	  "ml" 'clojure-move-to-let
	  "q"  'cider-quit
	  "rn" 'lsp-rename
	  "rr" 'lsp-find-references
	  "rc" 'lsp-treemacs-call-hierarchy
	  "sa" 'clojure-align
	  "sn" 'clojure-sort-ns
	  "sq" 'cider-format-defun
	  "te" 'lsp-clojure-show-test-tree
	  "tn" 'cider-test-run-ns-tests
	  "tp" 'cider-test-run-project-tests
	  "tt" 'cider-test-run-test
	  )
    :labels
    (""  "major mode"
	 "e" "eval"
	 "f" "format"
	 "a" "action"
	 "t" "test"
	 "l" "cider load"
	 "j" "repl"
	 "d" "documentation"))
  :config
  (setq lsp-clojure-server-command '("clojure-lsp")
	clojure-align-forms-automatically t
	org-babel-clojure-backend 'cider))

(module! eros
     :init
     (eros-mode 1)

     (defun eros-build-output-string (output value)
       (concat "" (when (not (string-empty-p output)) (concat output ": ")) value ))

     (defun eros-overlay-result (value)
       (eros--make-result-overlay (format "%s" value)
                      :where (save-excursion (end-of-defun) (point))
                      :duration eros-eval-result-duration))

     (defun ~/emacs-lisp/eval-defun-pp ()
       (interactive)
       (let ((output-buffer (get-buffer-create "*elisp-result*"))
         (current-buffer (current-buffer)))
         (save-excursion
              (end-of-defun)
              (beginning-of-defun)
              (let* ((form (read (current-buffer)))
             (result (eval form)))
        (pop-to-buffer output-buffer)
        (setq buffer-read-only nil)
        (erase-buffer)
        (pp result (current-buffer))
        (setq buffer-read-only t)))
         (pop-to-buffer current-buffer)))

     :config

     (comment
      (major-mode-map emacs-lisp-mode
              ;; TODO
              )))
(module! sly
     :after eros
     :init
     (defun sly-defun-at-point ()
       "Docs."
       (interactive)
       (save-excursion
        (apply #'buffer-substring-no-properties
           (sly-region-for-defun-at-point))))

     (defun sly-eval-overlay ()
       "Eval current expression in Lisp; insert any output in overlay at point."
       (interactive)
       (let ((form (sly-defun-at-point)))
         (if (string-match "^(defvar " form)
         (sly-eval-async
          `(slynk:re-evaluate-defvar ,form)
          (lambda (result)
            (setq this-command "eros-sly-eval-overlay")
            (eros-overlay-result result)))
         (sly-eval-async
          `(slynk:eval-and-grab-output ,form)
          (lambda (result)
            (setq this-command "eros-sly-eval-overlay")
            (cl-destructuring-bind (output value) result
                       (eros-overlay-result (eros-build-output-string output value))))))))
     (setq inferior-lisp-program "sbcl")


     (require 'sly-autodoc "contrib/sly-autodoc")
     (require 'sly-mrepl "contrib/sly-mrepl")

     (defun lisp/sly-mrepl-clear-repl ()
       (interactive)
       (save-window-excursion
         (switch-to-buffer "*sly-mrepl for sbcl*")
         (sly-mrepl-clear-repl)))
     :config
     (major-mode-map
         lisp-mode
      :bindings ("ed"
             'sly-eval-overlay
             "el" 'sly-eval-buffer
             "ep" 'sly-pprint-eval-last-expression
             "as" 'sly-toggle-trace-fdefinition
             "jc" 'sly-connect
             "q"  'sly-quit-lisp
             "c"  'lisp/sly-mrepl-clear-repl
             )
      :labels (;;"a" "debug"
           "j" "jack in"
               "e" "eval"))
     (major-mode-map
         sly-mrepl-mode
         :bindings ("c" 'sly-mrepl-clear-repl)))

           ;;;
;;;;;;;;;;;;;;

;;;;;;;
;;; SQL

(module! sql-indent
  :ensure t
  :defer t
  :init
  (add-hook 'sql-mode-hook 'sqlind-minor-mode))

(module! sqlformat
  :ensure t
  :defer t
  :config
  (setq sqlformat-command 'pgformatter
	sqlformat-args '("-s2" "-g")))

(module! sql
  :defer t
  :init
  (defun sql-product-string (sql-product)
    (cl-case sql-product
      ('postgres "postgresql")))

  (setq lsp-sqls-connections nil)
  (setq sql-connection-alist nil)
  (setq sql-connections nil)

  (defmacro sql-connection
      (connection-name product user password host port db)
    (let* ((server-connection `(,connection-name
				(sql-product ',product)
				(sql-user ,user)
				(sql-password ,password)
				(sql-server ,host)
				(sql-port ,port)
				(sql-database ,db)))
	   (lsp-string (format
			"host=%s port=%s user=%s password=%s dbname=%s sslmode=disable"
			host port user password db))

	   (lsp-connection `((driver . ,(sql-product-string product))
			     (dataSourceName . ,lsp-string))))
      `(progn
	 (push ',server-connection sql-connection-alist)
	 (push ',lsp-connection lsp-sqls-connections)
	 (push '(,connection-name ((lsp . ,lsp-connection)
				   (sqli . ,server-connection))) sql-connections))))

  (evil-define-key 'insert sql-mode-map
    (kbd "C-<return>") 'lsp-sql-execute-paragraph
    (kbd "C-c RET") 'lsp-sql-execute-paragraph
    (kbd "C-c C-c") 'lsp-sql-execute-paragraph)
  (evil-define-key 'normal sql-mode-map
    (kbd "C-<return>") 'lsp-sql-execute-paragraph
    (kbd "C-c RET") 'lsp-sql-execute-paragraph
    (kbd "C-c C-c") 'lsp-sql-execute-paragraph)

  (major-mode-map sql-mode
      :bindings
    ("c" 'lsp-sql-switch-connection
     "d" 'lsp-sql-switch-database))

  (sql-connection conure postgres "postgres" "postgres" "localhost" 5432 "investigation")
  (sql-connection scotus postgres "anparisi" "" "localhost" 5431 "kb"))

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

;;;;;;;;;;
;;; Docker

(module! docker
  :ensure t
  :defer t)


(module! dockerfile-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-mode)))

       ;;;
;;;;;;;;;;

;;;;;;;;;;;;;
;;; Utilities

(module! json-mode
  :defer t
  :ensure t)

(module! yaml-mode
  :defer t
  :ensure t)

(module! crdt
  :defer t
  :ensure t)

(module! proof-general
  :defer t
  :ensure t
  :init
  (add-hook 'coq-mode-hook (lambda () (undo-tree-mode 1)))
  (major-mode-map proof-mode-map
    "ed" 'proof-assert-next-command-interactive))

          ;;;
;;;;;;;;;;;;;

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


(comment
 "This is from the emacs show and tell 03-31-2023"
 (defun mail-work (start end &optional region?)
  (interactive "r\nP")
  (let ((s (buffer-substring start end)))
    (compose-mail "auhaas@cisco.com" "emacs region")
    (when region?
      (insert s))))

 "I should write this at some point, I'm surprised there aren't any
  emacs lisp packages for navigating clojure code."
  )

(module! forth-mode
  :defer t
  :ensure t
  :after eros
  :init
  (defun forth-expression-at-point ()
    (interactive)
    (save-excursion
      (re-search-forward "[;\\n]" nil t)
      (backward-sexp)
      (let ((start (point)))
	(forward-sexp)
	(buffer-substring-no-properties
	 start (point)))))

  )
