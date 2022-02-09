;;(require 'workspace)

(defvar *projects* '())

(defmacro defproject (project-name &rest args)
  (let* ((project-dir  (plist-get args :project-dir))
	 (conda-env     (plist-get args :conda-env))
	 (function-name (intern (->> project-name
				     (format "%s-session")))))
    (push `(,(symbol-name project-name) . ,function-name) *projects*)
    `(defun ,function-name (workspace-number)
       (interactive "sWorkspace Number: ")
       (let ((ws-num (string-to-number workspace-number)))
	 (delete-other-windows)
	 (dired ,project-dir)
	 (workspace--add-workspace-no-prompt
	  ws-num (format "{} %s" ,(symbol-name project-name)))
	 (conda-env-deactivate)
	 ,(when conda-env
	    `(conda-env-activate ,conda-env))))))

(defun project--all-projects ()
  (mapcar #'car  *projects*))

(defun project-switch-project (workspace-number)
  (interactive "sWorkspace Number: ")
  (let* ((project-name (ivy-read "Project: " (project--all-projects)))
	 (project-function
	  (alist-get project-name *projects* nil nil #'equal)))
    (funcall project-function workspace-number)))
