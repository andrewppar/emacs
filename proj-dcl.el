;;; proj-dcl.el --- Manage Eirene Projects -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 02 June 2023
;; Homepage: N/A
;; Keywords: projects
;; Package-Requires: ((emacs "28"))
;; SPDX-License-Identifier: MIT
;; Version: 3.0

;;; Commentary:

;; Declare Eirene Projects here
;;; Code:
(require 'tmux)

(defun kill-clojure-repls ()
  "Kill all running clojure repls."
  (interactive)
  (tmux-kill-all-windows-with-name "rlwrap")
  (tmux-kill-all-windows-with-name "bb"))

(defun start-incident-manager ()
  "Run the incident manager in another tmux window."
  (interactive)
  (tmux-new-window-with-process "incident_manager" "incident-manager"))

(defun potentially-start-cider (project-name)
  "Connect to PROJECT-NAME's repl if it's up."
  (require 'cider)
  (let* ((host "localhost")
	 (port-alist (cider--infer-ports host nil)))
    (when-let ((port (car
		      (alist-get
		       project-name port-alist nil nil #'equal))))
      (let ((params (-> '()
			(plist-put :host host)
			(plist-put :port port))))
	(my-cider-op 'cider-connect-clj params))
      t)))

(defun conure-init ()
  "Start a conure session."
  (interactive)
  (tmux-new-window-with-process "conure" "conure")
  (sleep-for 4)
  (when (potentially-start-cider "conure")
    (shell-command "docker compose up -d db")
    (shell-command "docker compose up -d fake-ctia")
    (shell-command "docker compose up -d distributor")
    (cider-load-file
     "/Users/anparisi/projects/conure/scratch/startup.clj")
    ;; it would be cooler to do something like this...
    ;;(cider-interactive-eval "(conure.startup/start-dev)")
    ))

(defun conure-quit ()
  "Close conure repl and surrounding processes."
  (interactive)
  (save-all-buffers)
  (when (or
	 (sesman-browser-get 'object)
	 (cider-current-repl))
    (cider-quit))
  (let* ((containers
	  (split-string (shell-command-to-string "docker ps") "\n"))
	 (conure-db (let ((result nil))
		      (dolist (row containers)
			(let ((items (split-string row)))
			  (when (equal (cadr items) "postgres:14")
			    (setq result (car items)))))
		      result)))
    (when conure-db
      (shell-command (format "docker stop %s" conure-db))))
  (tmux-kill-all-windows-with-name "conure")
  (tmux-kill-all-windows-with-name "incident-manager"))

(defproject conure
    :key "c"
    :website "https://github.com/advthreat/conure"
    :stop (progn (conure-quit) (shell-command "docker compose down"))
    :commands ((:title "db" :executable "just"
		       :args ("db") :transient-key "d")
	       (:title "stop" :executable conure-quit
		       :transient-key "q" :executor :elisp)
	       (:title "start" :executable conure-init
		       :executor :elisp :transient-key "s")
	       (:title "incident manager" :executable start-incident-manager
		       :executor :elisp :transient-key "i")
	       (:title "test" :executable "just"
		       :args ("test") :transient-key "t")
	       (:title "integration test" :executable "just"
		       :args ("test-ci") :transient-key "I")
	       (:title "reset db" :executable "just"
		       :args ("db-reset") :transient-key "r")
	       (:title "migrate db" :executable "just"
		       :args ("db-migrate") :transient-key "m"))
    :project-dir "/Users/anparisi/projects/conure")

(defproject git-sync
    :key "g"
    :project-dir "/Users/anparisi/projects/git-sync")

(defproject organizer
    :key "o"
    :project-dir "/Users/anparisi/org/"
    :commands ((:title "Log Task" :executable log-task!
		       :transient-key "t" :executor :elisp)
	       (:title "Restore" :executable restore-organizer
		       :transient-key "r" :executor :elisp))
    :init (restore-organizer))

(defun scotus-init ()
  "Initialize a Scotus Session."
  (interactive)
  (tmux-new-window-with-process "scotus" "scotus")
  (sleep-for 4)
  (when (potentially-start-cider "scotus")
    (shell-command "brew services start postgresql@14")
    (cider-load-file
     "/Users/anparisi/projects/scotus/scratch/startup.clj")))

(defun scotus-quit ()
  "Quit a scotus session."
  (interactive)
  (cider-quit)
  (tmux-kill-all-windows-with-name "scotus"))

(defproject scotus
    :key "s"
    :init (scotus-init)
    :commands ((:title "test" :executable "clj" :args ("-X:dev/test")
		       :transient-key "t"))
    :project-dir "/Users/anparisi/projects/scotus"
    :commands ((:title "test" :executable "clj"
		       :args ("-X:dev/test") :transient-key "t"))
    :stop )

(defproject sql
    :key "q"
  :init (sql-session-start))

(defproject teamspace
    :key "t"
    :init (teamspace-start-session)
    :stop (save-all-buffers))

(project-switch-transient)

(provide 'proj-dcl)
;;; proj-dcl.el ends here
