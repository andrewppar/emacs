;;; project.el -- Eirene Project Sessions

   ;; Copyright (C) 2022  Andrew Parisi

   ;; Author: Andrew Parisi <andrew.p.parisi@gmail.com
   ;; Keywords: lisp
   ;; Version: 0.0.1

   ;; This program is free software; you can redistribute it and/or modify
   ;; it under the terms of the GNU General Public License as published by
   ;; the Free Software Foundation, either version 3 of the License, or
   ;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
   ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
   ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   ;; GNU General Public License for more details.

   ;; You should have received a copy of the GNU General Public License
   ;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

    ;; A package for managing projects making
    ;; particular use of the workspace package

;;; Code:

;;(require 'workspace)
(require 'transient)
(require 'ansi-color)

;; Make this into a single hash table
(defvar *project-projects* '())
(defvar *project-transient-keys* '())
(defvar *project-workspace-map* '())
(defvar *project-directories* '())
(defvar *project-quit-function* ())


(defun alist-update (alist key update-function &rest args)
  "Change the value of ALIST at KEY with UPDATE-FUNCTION applied to ARGS."
  (let* ((old-value (alist-get key alist nil t #'equal))
	 (new-alist (assoc-delete-all key alist #'equal))
	 (new-value (apply update-function old-value args)))
    (cons (cons key new-value) new-alist)))

(defun plist-keys (plist)
  "Get the keys for PLIST."
  (let ((key-position? t)
	(result      '()))
    (dolist (item plist)
      (when key-position?
	(push item result))
      (setq key-position? (not key-position?)))
    result))

(defun create-run-function (project-dir project-name command)
  "Create a project run function for PROJECT-NAME in PROJECT-DIR using COMMAND."
  (let ((function-name (intern (format "run-%s" project-name))))
    `(defun ,function-name ()
       (interactive)
       (vterm)
       (vterm--goto-line -1)
       ;;(vterm-send-string (vterm-get-password))
       ;;(vterm-send-return)
       ;;(vterm-clear)
       (vterm-send-command (format "cd %s" ,project-dir))
       (vterm-send-command ,command))))

(defun docker-build-function (project-dir tag vault build-block)
  "Create a function to build a docker image in PROJECT-DIR.

 Build  with TAG in AWS VAULT with the options in BUILD-BLOCK."
  (let* ((tag-name   (substring (symbol-name tag) 1))
	 (function-name (intern (format "%s-build" tag-name)))
	 (build-args    (plist-get build-block :build-args))
	 (file          (plist-get build-block :file))
	 (cd-string     (format "cd %s" project-dir))
	 (build-string (if vault
			   (format
			    "aws-vault exec %s -- docker build" vault)
			 (format "docker build"))))
    (dolist (build-arg build-args)
      (setq build-string
	    (format "%s --build-arg %s" build-string build-arg)))
    (setq build-string
	  (format "%s -t %s" build-string tag-name))
    (when file
      (setq build-string
	    (format "%s -f %s" build-string file)))
    (setq build-string
	  (format "%s ." build-string))
    `(defun ,function-name ()
       (interactive)
       (vterm)
       (vterm--goto-line -1)
       (vterm-send-command ,cd-string)
       (vterm-send-command ,build-string))))

(defun docker-run-function (project-dir tag vault run-block)
  "Create a function to run a docker image in PROJECT-DIR.

It is run with TAG in AWS VAULT using RUN-BLOCK."
  (let* ((tag-name      (substring (symbol-name tag) 1))
	 (function-name (intern (format "%s-run" tag-name)))
	 (volume        (plist-get run-block :volume))
	 (envs          (plist-get run-block :environment))
	 (command       (plist-get run-block :command))
	 (ports         (plist-get run-block :ports))
	 (mount         (plist-get run-block :mount))
	 (network       (plist-get run-block :network))
	 (cd-string     (format "cd %s" project-dir))
	 (run-string (if vault
			 (format
			  "aws-vault exec %s -- docker run" vault)
		       (format "docker run"))))
    (when volume
      (setq run-string
	    (format "%s -v %s" run-string volume)))
    (when mount
      (let ((type   (plist-get mount :type))
	    (source (plist-get mount :source))
	    (target (plist-get mount :target)))
	(setq run-string
	      (format "%s --mount type=%s,source=%s,target=%s"
		      run-string type source target))))
    (when network
      (setq run-string
	    (format "%s --network=%s" run-string network)))
    (dolist (port ports)
      (setq run-string
	    (format "%s -p%s:%s"
		    run-string (car ports) (cdr ports))))
    (dolist (env envs)
      (setq run-string
	    (format "%s -e %s" run-string env)))
    (setq run-string
	  (format "%s %s %s" run-string tag-name command))
    `(defun ,function-name ()
       (interactive)
       (vterm)
       (vterm--goto-line -1)
       (vterm-send-command ,cd-string)
       (vterm-send-command ,run-string))))


(defun create-command-symbol (project-name title)
  "Create a command for PROJECT-NAME with TITLE."
  (let ((clean-title
	 (replace-regexp-in-string (regexp-quote " ") "-" title)))
    (intern (format "%s-%s" project-name clean-title))))

(define-minor-mode project-command-mode
    "Minor mode for project command result pane."
  :init-value nil)
(evil-define-key 'normal project-command-mode-map
  "q" 'kill-buffer-and-window)

(defun create-command-function (project-name project-dir command)
  "Create a function to run COMMAND in PROJECT-DIR with PROJECT-NAME."
  (cl-destructuring-bind (&key title executable args &allow-other-keys)
      command
    (let ((function-name       (create-command-symbol project-name title))
	  (project-name-string (format "%s" project-name))
	  (executor            (or (plist-get command :executor) :shell)))
      `(defun ,function-name ()
	 (interactive)
	 (let ((default-directory ,project-dir))
	   ,(cl-case executor
	     (:elisp `(,executable ,@args))
	     (:shell
	      `(progn
		 (when (< (length (window-list)) 2)
		   (split-window-right))
		 (let ((test-buf ,(format "*%s*" function-name)))
		   (with-output-to-temp-buffer test-buf
		     (switch-to-buffer test-buf)
		     (project-command-mode 1)
		     (call-process ,executable nil test-buf t ,@args)
		     (ansi-color-apply-on-region
		      (point-min) (point-max))))))))))))

(defun create-command-functions (project-name project-dir commands)
  "Create command functions for PROJECT-NAME in PROJECT-DIR with COMMANDS."
  `(progn
     ,@(mapcar
	(lambda (command)
	  (create-command-function project-name project-dir command))
       commands)))

(defun command-transient-menu-item (project-name command)
  "Create a transient menu item for COMMAND in PROJECT-NAME."
  (cl-destructuring-bind (&key title transient-key &allow-other-keys)
      command
    (let ((function-name (create-command-symbol project-name title)))
      `(,(format "c%s" transient-key) ,title ,function-name))))

(defun command-transient-menu-items (project-name commands)
  "Create all transient menu items for COMMANDS in PROJECT-NAME."
  (mapcar
   (lambda (command)
     (command-transient-menu-item project-name command))
   commands))

(defun create-docker-functions (project-dir args)
  "Creat all docker functions for PROJECT-DIR with ARGS."
  (let ((docker-block (plist-get args :docker)))
    (when docker-block
      (let ((tags (plist-keys docker-block))
	    (result '()))
	(dolist (tag tags)
	  (let* ((tag-block   (plist-get docker-block tag))
		 (build-block (plist-get tag-block :build))
		 (run-block   (plist-get tag-block :run))
		 (vault       (plist-get tag-block :vault)))
	    (push
	     (docker-build-function project-dir tag vault build-block) result)
	    (push
	     (docker-run-function project-dir tag vault run-block) result)))
	(cons 'progn result)))))

(defun regenerate-docker-function-names (docker-block)
  "Get the docker function names for DOCKER-BLOCK."
  (let ((tags    (plist-keys docker-block))
	(result '()))
    (dolist (tag tags)
      (let* ((base-name (substring (symbol-name tag) 1))
	     (run-name (intern (format "%s-run" base-name)))
	     (build-name (intern (format "%s-build" base-name))))
	(push run-name result)
	(push build-name result)))
    result))

(defun create-docker-compose-functions (project-dir project-name)
  "Create all the docker-compose functions for PROJECT-NAME with PROJECT-DIR."
  (let ((build (intern (format "compose-build-%s" project-name)))
	(run   (intern (format "compose-run-%s" project-name))))
    `(progn
       (defun ,build ()
	 (interactive)
	 (vterm)
	 (vterm--goto-line -1)
	 (vterm-send-command (format "cd %s" ,project-dir))
	 (vterm-send-command "docker compose build"))
       (defun ,run ()
	 (interactive)
	 (vterm)
	 (vterm--goto-line -1)
	 (vterm-send-command (format "cd %s" ,project-dir))
	 (vterm-send-command "docker compose up")))))


  (defun project-letter-inc (letter)
  (let ((letters '("a" "b" "c" "d" "e" "f" "g"
		  "h" "i" "j" "k" "l" "m" "n"
		  "o" "p" "q" "r" "s" "t" "u"
		  "v" "w" "x" "y" "z"))
	(letter-map '()))
    (cl-do ((key  (car letters)  (car todo))
	    (val  (cadr letters) (cadr todo))
	    (todo (cdr letters) (cdr todo)))
	((not val) letter-map)
      (push (cons key val) letter-map))
    (alist-get letter letter-map nil nil 'equal)))

(defmacro defproject (project-name &rest args)
  "Macro for project declaration.

  Takes a PROJECT-NAME and keyword ARGS followed by
  a value.  Possible keywords are:

   :project-dir -- a specification of where the project is
   :conda-env   -- a specification of a conda environment
     associated with the project."
  ;; todo - make this a descructuring
  (let* ((project-dir     (plist-get args :project-dir))
	 (conda-env       (plist-get args :conda-env))
	 (init            (plist-get args :init))
	 (transient-key   (plist-get args :key))
	 (stop            (plist-get args :stop))
	 (docker          (plist-get args :docker))
	 (run-command     (plist-get args :run))
	 (website         (plist-get args :website))
	 (docker-compose  (plist-get args :docker-compose))
	 (commands        (plist-get args :commands))
	 (start-function-name (intern (->> project-name
					   (format "%s-session"))))
	 (dir-function-name   (intern (->> project-name
					   (format "%s-session-dired"))))
	 (website-function-name (intern (->> project-name
					     (format "%s-session-visit-webiste")))))
    (push `(,(symbol-name project-name) . ,start-function-name)
	  *project-projects*)
    (push `(,(symbol-name project-name) . ,transient-key)
	  *project-transient-keys*)
    (when stop
      (push (cons (format "%s" project-name) `,stop)
    	    *project-quit-function*))
    `(progn
       (defun ,start-function-name ()
	 (interactive)
	 (let ((ws-num (workspace-get-next-workspace-number)))
	 (delete-other-windows)
	 (workspace-to-workspace-number-with-name
	  ws-num (format "{} %s" ,(symbol-name project-name)))
	 (push (cons ws-num ,(format "%s" project-name)) *project-workspace-map*)
	 ,(when project-dir
	    (setq *project-directories*
		  (alist-update *project-directories*
				(format "%s" project-name)
				(lambda (value)
				  (identity value))))
	    `(progn
	       (dired ,project-dir)))
	 ,(when conda-env
	    `(progn
	       (pyvenv-deactivate)
	       (pyvenv-workon ,conda-env)))
	 ,(when init
	    `(progn
	       ,init))
	 (worskpace-save-current-view)))

       (defun ,dir-function-name ()
	 (interactive)
	 (dired ,project-dir))
       ,(when docker
	  (create-docker-functions project-dir args))
       ,(when docker-compose
       	  (create-docker-compose-functions project-dir project-name))
       ,(when run-command
	  (create-run-function project-dir project-name run-command))
       ,(when commands
	  (create-command-functions project-name project-dir commands))
       ,(when website
	  `(defun ,website-function-name ()
	     (interactive)
	     (browse-url ,website)))
       ,(let ((transient-command '()))
	  (when docker
	    (let ((fns (regenerate-docker-function-names docker))
		  (current-letter  "a"))
	      (dolist (fn fns)
		(let ((command (format "d%s" current-letter)))
		  (push
		   `(,command ,(format "%s" fn) ,fn) transient-command)
		  (setq current-letter
			(project-letter-inc current-letter))))))
	  (setq transient-command (reverse transient-command))
	  (when website
	    (push `("v" "visit webiste" ,website-function-name)
		  transient-command))
	  (when run-command
	    (push `("g" "run" ,(intern (format "run-%s" project-name)))
		  transient-command))
	  (when commands
	    (let ((menu-items (command-transient-menu-items
			       project-name commands)))
	    (dolist (menu-item menu-items)
	      (push menu-item transient-command))))
	  (when docker-compose
	    (push
	     `("cb" "compose build"
	       ,(intern (format "compose-build-%s" project-name)))
	     transient-command)
	    (push
	     `("cr" "compose run"
	       ,(intern (format "compose-run-%s" project-name)))
	     transient-command))
	  `(transient-define-prefix ,project-name ()
	    ,(format "Transient for %s" project-name)
	    [,(format "Menu: %s" project-name)
	     ("f" "dired" ,dir-function-name)
	     ,@transient-command])))))

(defun project--all-projects ()
  "Gather all project names."
  (reverse
   (mapcar #'car *project-projects*)))

(defmacro project-switch-transient ()
  "A macro to dynamically create transients for the project switcher."
  (let ((menu-items '()))
    (dolist (project (project--all-projects))
      (let ((fn (alist-get project *project-projects* nil nil #'equal))
	    (key (alist-get project *project-transient-keys*)))
	(push (list key project fn) menu-items)))
    `(transient-define-prefix project-switch-project ()
      "Allow user to switch to a project."
      ["Project" ,@(reverse menu-items)])))

(defun project-transient ()
  (interactive)
  (if-let ((project-name (alist-get *current-workspace* *project-workspace-map*)))
      (funcall (intern project-name))
    (message "No project associated with current workspace")))

(defun project--name-from-workspace (ws)
  (-> ws (split-string ":") cdr car (substring 1)))

(defun project-quit-project ()
  "Select a project to remove the workspace and all buffers for."
  (interactive)
  (let ((all-projects      (project--all-projects))
	(active-workspaces (workspace-list))
	(active-projects   '()))
    (dolist (workspace active-workspaces)
      (let  ((workspace-name (project--name-from-workspace workspace)))
	(when (member workspace-name all-projects)
	  (push workspace active-projects))))
    (let* ((to-quit (ivy-read "Project: " (reverse active-projects)))
	   (ws-name (project--name-from-workspace to-quit))
	   (stop (alist-get
		  ws-name *project-quit-function* nil nil #'equal)))
      (save-workspace-excursion ws-name
	(eval stop))
      (workspace-remove-workspace to-quit))))

(provide 'eirene-project)
;;; project.el ends here
