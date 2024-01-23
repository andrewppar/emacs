;; proj-dcl.el --- Manage Eirene Projects -*- lexical-binding: t -*-

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
(require 'tmuxmacs)

(defun phoenix-build ()
  "Build Phoenix."
  (interactive)
  (let ((default-directory "/Users/anparisi/.config/phoenix"))
    (call-process "just" nil nil t "build")))

(defproject phoenix
    :key "s"
    :project-dir "/Users/anparisi/.config/phoenix"
    :commands ((:title "build" :executable "just"
		      :args ("build")
		      :transient-key "b")))


(defun potentially-start-cider (project-name)
       "Connect to PROJECT-NAME's repl if it's up."
       (require 'cider)
       (let* ((host "localhost")
	      (port-alist (cider--infer-ports host nil)))
	 (when-let ((port (car
			   (alist-get
			    project-name port-alist nil nil #'equal))))
	   (let ((params
		  (plist-put (plist-put '() :host host) :port port)))
	     (my-cider-op 'cider-connect-clj params))
	   t)))

(defun scotus-init ()
  "Initialize a Scotus Session."
  (interactive)
  (let ((session (tmux-session/find-or-make "background")))
    (if-let ((window (tmux-window/find "scotus")))
	(message "scotus is already running.")
      (let* ((window (tmux-window/make "scotus" session))
	     (pane (car (tmux-pane/list window))))
	(tmux-pane/send-command pane "scotus")
	(sleep-for 4)
	(when (potentially-start-cider "scotus")
	  (shell-command "brew services start postgresql@16")
	  (cider-interactive-eval
	   "(do (require '[scotus.core :as scotus]) (scotus/init!))"))))))

(defun scotus-quit ()
  "Quit a scotus session."
  (interactive)
  (cider-quit)
  (tmux-window/kill (tmux-window/find "scotus")))

(defproject scotus
    :key "s"
    :init (scotus-init)
    :commands ((:title "test" :executable "clj" :args ("-X:dev/test")
		       :transient-key "t"))
    :project-dir "/Users/anparisi/projects/scotus"
    :commands ((:title "stop" :executable :elisp
		       :executor scotus-quit
		       :transient-key "q")
	       (:title "start" :executable :elisp
		       :executor scotus-init
		       :transient-key "s")
	       (:title "test" :executable "clj"
		       :args ("-X:dev/test") :transient-key "t"))
    :stop (scotus-quit))

(defproject sql
    :key "q"
    :init (sql-session-start))

(defproject bash-parens
    :key "b"
    :project-dir "/Users/anparisi/quicklisp/local-projects/bash-parens"
    :init (let* ((make-server "(slynk:create-server :port 4005 :dont-close t)"))
	    (tmux-new-window-with-process
	     (format "rlwrap sbcl --eval '%s'" make-server)
	     "lisp")
	    (sleep-for 2)
	    (sly-connect "localhost" 4005)
	    (sly-interactive-eval "(ql:quickload :persidastricl)")
	    (find-file
	     "/Users/anparisi/quicklisp/local-projects/bash-parens/src/main.lisp"))
  :commands ((:title "test" :executable "sbcl"
	      :args ("--eval " "\"(asdf:test-system :bash-parens)\"")
		     :transient-key "t"))
  :quit (sly-quit-lisp))

(defun pipes-init ()
  "Start a pipes session."
  (interactive)
  (message "starting pipes backend...")
  (let* ((session (tmux-session/find-or-make "background"))
	 (window  (tmux-window/make "pipes" session))
	 (pane    (car (tmux-pane/list window))))
    (tmux-pane/send-command
     pane
     "cd /Users/anparisi/projects/pipes ; clj -M:dev/repl")
    (sleep-for 3)
    (message "Trying to connect to backend...")
    (if (potentially-start-cider "pipes")
	(cider-interactive-eval
	 "(do (require '[pipes.server :as server]) (server/run!))")
      (message "Could not find cider session for pipes.")
      (message "starting pipes frontend..."))
    (let ((next-pane (tmux-pane/split pane)))
      (tmux-pane/send-command
       next-pane
       "cd /Users/anparisi/projects/pipes; npx shadow-cljs watch :app"))))

(defun pipes-quit ()
  "Stop a pipes session."
  (interactive)
  (if-let ((window (tmux-window/find "pipes")))
      (tmux-window/kill window)
    (message "No pipes session running. ")))

(defproject pipes
    :key "p"
    :project-dir "/Users/anparisi/projects/pipes"
    :commands ((:title
		"start"
		:executable pipes-init
		:transient-key "s"
		:executor :elisp)
	       (:title
		"stop"
		:executable pipes-quit
		:transient-key "q"
		:executor :elisp))
    :quit (pipes-stop))

(defproject organizer
    :key "o"
    :project-dir "/Users/anparisi/org"
    :init (restore-organizer))

(project-switch-transient)

(provide 'proj-dcl)
;;; proj-dcl.el ends here
