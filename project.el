(require 'projectile)

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
	 ,(when conda-env
	    `(conda-env-activate ,conda-env))))))
