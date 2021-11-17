(require 'ivy)

(defvar *workspaces* '())
(defvar *current-workspace* nil)

(defun workspace--add-ivy-view (view-name)
  ;;TODO: This should be named add-or-update...
  (let ((view (ivy--get-view-config))
	(x (assoc view-name ivy-views)))
    (if x
        (setcdr x (list view))
      (push (list view-name view) ivy-views))))

(defun workspace-switch-workspace (ws-name)
  (when *current-workspace*
    (let ((current-ws-name
	   (cdr (assoc *current-workspace* *workspaces*))))
      (workspace--add-ivy-view current-ws-name)))
  (setq
   *current-workspace* (workspace-key-from-name ws-name))
  (ivy--switch-buffer-action ws-name))

(defun workspace-list-workspace-names ()
  (let ((result '()))
    (dolist (entry *workspaces*)
      (push (cdr entry) result))
    result))

(defun workspace-list-workspace-keys ()
  (let ((result '()))
    (dolist (entry *workspaces*)
      (push (car entry) result))
    result))

(defun workspace-key-from-name (ws-name)
  (cl-do ((next-items (cdr *workspaces*)
		      (cdr next-items))
	  (current-item (car *workspaces*)
			(car next-items)))
      ((or (equal (cdr current-item) ws-name)
	   (not next-items))
       (when (equal (cdr current-item) ws-name)
	 (car current-item)))
    nil))

(defun workspace-add-workspace (n)
  (let ((name
	 (ivy-read
	  "Name workspace: "
	  nil
	  :initial-input (ivy-default-view-name))))
    (setq *current-workspace* n)
    (push (cons n name) *workspaces*)
    (workspace--add-ivy-view name)))

(defun workspace-to-workspace-number (n)
  (when (<= n 0)
    (error "Workspace number must be at least 1."))
  (if-let ((ws-name (cdr (assoc n *workspaces*))))
      (workspace-switch-workspace ws-name)
    (workspace-add-workspace n)))

(defun workspace--remove-workspace (n)
  (setq *workspaces*
	(assoc-delete-all n *workspaces*)))

(defun workspace-pop ()
  (interactive)
  (let* ((workspaces (workspace-list-workspace-names))
	 (to-remove  (ivy-read
		      "Pop Workspace: "
		      workspaces))
	 (ws-number  (workspace-key-from-name to-remove)))
    (workspace--remove-workspace ws-number)
    (ivy-pop-view-action (assoc to-remove ivy-views))))
