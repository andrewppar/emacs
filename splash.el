(require 'seq)

(load! "~/.emacs.d/project.el")

(defun display-eirene-banner-graphics ()
  (insert " _____                               \n")
  (insert "|  ___)                              \n")
  (insert "| |_    _    ___   _ ")
  (insert-char #x200b) (insert "_")
  (insert-char #x200b) (insert "_   _  ")
  (insert-char #x200b) (insert "_")
  (insert-char #x200b) (insert "_ _ __ \n")
  (insert "|  _)  | |  / _ \\ | '_ \\ | |/ /| '_ \\ \n")
  (insert "| |___ | | | |_) )| | | || / / | | | |\n")
  (insert "|_____) \\_)|  __/ |_| | ||__/  |_| | |\n")
  (insert "           | |        | |          | |\n")
  (insert "           |")
  (insert-char #x200b)
  (insert "_|        |_|          |_|\n"))

(defun display-eirene-banner-terminal ()
  (insert "#+BEGIN_SRC\n")
  (insert " _____                               \n")
  (insert "|  ___)                              \n")
  (insert "| |_    _    ___   _ __   _  __ _ __ \n")
  (insert "|  _)  | |  / _ \\ | '_ \\ | |/ /| '_ \\ \n")
  (insert "| |___ | | | |_) )| | | || / / | | | |\n")
  (insert "|_____) \\_)|  __/ |_| | ||__/  |_| | |\n")
  (insert "           | |        | |          | |\n")
  (insert "           |_|        |_|          |_|\n")
  (insert "#+END_SRC\n"))

(defvar *splash-projects-point* nil)

(defun goto-projects ()
  (interactive)
  (goto-char *splash-projects-point*)
  (forward-line 2))

(defun display-projects ()
  (let ((projects '()))
    (dolist (alist *project-projects*)
      (push (car alist) projects))
    (setq *splash-projects-point* (point))
    (when projects
      (insert "Projects (p): \n\n")
      (let ((index 1))
	(dolist (project projects)
	  (insert (format "%s - %s \n" index project))
	  (setq index (+ index 1)))))))

(defvar *splash-recents-point* nil)

(defun goto-recents ()
  (interactive)
  (goto-char *splash-recents-point*)
  (forward-line 2))

(defun display-recent-files ()
  (recentf-mode)
  (let ((recent-files (seq-take recentf-list 7))
	(index 1))
    (setq *splash-recents-point* (point))
    (insert "Recent Files (r): \n\n")
    (dolist (path recent-files)
      (insert (format "%s - [[%s]] \n" index path))
      (setq index (+ index 1)))
    (insert "\n")))

(defun display-keybindings ()
  (let ((bindings '()))
    (push
     `("find-file" . ,(substitute-command-keys "\\[counsel-find-file]"))
     bindings)
    (push
     `("M-x" . ,(substitute-command-keys "\\[counsel-M-x]"))
     bindings)
    (push
     `("Switch Project" . ,(substitute-command-keys
			    "\\[project-switch-project]"))
     bindings)
    (insert "Key Bindings: \n\n")
    (insert "|-----+---------|\n")
    (insert "| key | binding |\n")
    (insert "|-----+---------|\n")
    (dolist (key bindings)
      (insert (format "|%s| \"%s\"|\n" (car key) (cdr key))))
    (insert "|-----+--------|")
    (org-table-align)
    (insert "\n\n")))

(defvar eirene-splash-mode-map
  (make-sparse-keymap))

(define-minor-mode eirene-splash-mode
  "Minor mode for eirene splash"
  :init-value nil
  :lighter " eirene splash "
  :keymap eirene-splash-mode-map)

(evil-define-key 'normal eirene-splash-mode-map (kbd "r") 'goto-recents)
(evil-define-key 'normal eirene-splash-mode-map (kbd "p") 'goto-projects)
(evil-define-key 'normal eirene-splash-mode-map (kbd "RET") 'org-open-at-point)

(defun eirene-splash (load-time)
  (switch-to-buffer "*Eirene Splash*")
  (let ((start (point-min))
	(end   (point-max)))
    (org-mode)
    (kill-region start end)
    (display-eirene-banner-terminal)
    (insert "\n\n\n")
    (insert (format "Load time: %s" load-time))
    (insert "\n\n\n")
    (display-keybindings)
    (insert "\n")
    (display-projects)
    (insert "\n")
    (display-recent-files)
    (goto-char start)
    (read-only-mode)
    (when (display-graphic-p)
      (local-set-key (kbd "<return>") #'org-open-at-point))))
