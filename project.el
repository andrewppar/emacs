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
(defvar *project-projects* '())
(defvar *project-website-map* '())
(defvar *project-directories* '())
(defvar *project-quit-function* ())


(defun alist-update (alist key update-function &rest args)
  (let* ((old-value (alist-get key alist nil t #'equal))
	 (new-alist (assoc-delete-all key alist #'equal))
	 (new-value (apply update-function old-value args)))
    (cons (cons key new-value) new-alist)))

(defmacro defproject (project-name &rest args)
  "Macro for project declaration.

  Takes a PROJECT-NAME and keyword ARGS followed by
  a value.  Possible keywords are:

   :project-dir -- a specification of where the project is
   :conda-env   -- a specification of a conda environment
     associated with the project."
  (let* ((project-dir  (plist-get args :project-dir))
	 (conda-env    (plist-get args :conda-env))
	 (init         (plist-get args :init))
	 (stop         (plist-get args :stop))
	 (website      (plist-get args :website))
	 (function-name (intern (->> project-name
				     (format "%s-session")))))
    (push `(,(symbol-name project-name) . ,function-name)
	  *project-projects*)
    (when stop
      (push (cons (format "%s" project-name) `,stop)
    	    *project-quit-function*))
    `(defun ,function-name (ws-num)
       (interactive "nWorkspace Number: ")
       (delete-other-windows)
       (workspace--add-workspace-no-prompt
	ws-num (format "{} %s" ,(symbol-name project-name)))
       ,(when project-dir
	  (setq *project-directories*
		(alist-update *project-directories*
			      (format "%s" project-name)
			      (lambda (value)
				(identity value))))
		`(dired ,project-dir))
	  ,(when conda-env
	     `(progn
		(conda-env-deactivate)
		(conda-env-activate ,conda-env)))
	  ,(when website
	     (push `(,(format "%s" project-name) . ,website)
		   *project-website-map*)
	     nil)
	  ,(when init
	     `(progn
		,init)
	     )
	  )
       ))

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
    (funcall project-function workspace-number)))

(defun project-browse-website ()
  "Select a project website to visit.

  Options are selected from the projects
  that specify a :website keyword."
  (interactive)
  (let* ((websites (mapcar #'car *project-website-map*))
	 (to-visit (ido-completing-read
		    "Select a website: "
		    websites
		    nil
		    t))
	 (url      (alist-get
		    to-visit *project-website-map* nil nil #'equal)))
    (browse-url url)))

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
