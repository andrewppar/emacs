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

(defvar *project-projects* '())
(defvar *project-workspace-map* '())
(defvar *project-directories* '())
(defvar *project-quit-function* ())


(defun alist-update (alist key update-function &rest args)
  (let* ((old-value (alist-get key alist nil t #'equal))
	 (new-alist (assoc-delete-all key alist #'equal))
	 (new-value (apply update-function old-value args)))
    (cons (cons key new-value) new-alist)))

(defun plist-keys (plist)
  (let ((key-position? t)
	(result      '()))
    (dolist (item plist)
      (when key-position?
	(push item result))
      (setq key-position? (not key-position?)))
    result))

(defmacro with-vterm(&rest body)
  (declare (indent defun))
  (vterm)
  (vterm--goto-line -1)
  (vterm-send-string (vterm-get-password))
  (vterm-send-return)
  (vterm-clear)
  (progn ,@body))

(defun create-run-function (project-dir project-name command)
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
;;       (vterm-send-string (vterm-get-password))
;;       (vterm-send-return)
;;       (vterm-clear)
       (vterm-send-command ,cd-string)
       (vterm-send-command ,build-string))))

(defun docker-run-function (project-dir tag vault run-block)
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
       ;;(vterm-send-string (vterm-get-password))
       ;;(vterm-send-return)
       ;;(vterm-clear)
       (vterm-send-command ,cd-string)
       (vterm-send-command ,run-string))))

(defun create-docker-functions (project-dir args)
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
  (let* ((project-dir     (plist-get args :project-dir))
	 (conda-env       (plist-get args :conda-env))
	 (init            (plist-get args :init))
	 (stop            (plist-get args :stop))
	 (docker          (plist-get args :docker))
	 (run-command     (plist-get args :run))
	 (website         (plist-get args :website))
	 (docker-compose  (plist-get args :docker-compose))
	 (start-function-name (intern (->> project-name
					   (format "%s-session"))))
	 (dir-function-name   (intern (->> project-name
					   (format "%s-session-dired"))))
	 (website-function-name (intern (->> project-name
					     (format "%s-session-visit-webiste")))))
    (push `(,(symbol-name project-name) . ,start-function-name)
	  *project-projects*)
    (when stop
      (push (cons (format "%s" project-name) `,stop)
    	    *project-quit-function*))
    `(progn
       (defun ,start-function-name (ws-num)
	 (interactive "nWorkspace Number: ")
	 (delete-other-windows)
	 (workspace-to-workspace-number-with-name
	  ws-num (format "{} %s" ,(symbol-name project-name)))
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
	 (worskpace-save-current-view))

       (defun ,dir-function-name ()
	 (interactive)
	 (dired ,project-dir))
       ,(when docker
	  (create-docker-functions project-dir args))
       ,(when docker-compose
       	  (create-docker-compose-functions project-dir project-name))
       ,(when run-command
	  (create-run-function project-dir project-name run-command))
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
  (mapcar #'car *project-projects*))


(defun project-switch-project ()
  "Allow user to switch to a project.

   User specifies the PROJECT, the highest workspace available is used."
  (interactive)
  (let* ((workspace-number (workspace-get-next-workspace-number))
	 (project-name     (ivy-read "Project: " (project--all-projects)))
	 (project-function (alist-get
			    project-name *project-projects* nil nil #'equal)))
    (push (cons workspace-number project-name) *project-workspace-map*)
    (funcall project-function workspace-number)))

(defun project-transient ()
  (interactive)
  (if-let ((project-name (alist-get *current-workspace* *project-workspace-map*)))
      (funcall (intern project-name))
    (message "No project associated with current workspace")))

;;(defun project-browse-website ()
;;  "Select a project website to visit.
;;
;;  Options are selected from the projects
;;  that specify a :website keyword."
;;  (interactive)
;;  (let* ((websites (mapcar #'car *project-website-map*))
;;	 (to-visit (ido-completing-read
;;		    "Select a website: "
;;		    websites
;;		    nil
;;		    t))
;;	 (url      (alist-get
;;		    to-visit *project-website-map* nil nil #'equal)))
;;    (browse-url url)))

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
	   (stop (alist-get
		  (project--name-from-workspace to-quit)
       		  *project-quit-function* nil nil #'equal)))
      (eval stop)
      (workspace-remove-workspace to-quit))))

(provide 'eirene-project)
;;; project.el ends here
