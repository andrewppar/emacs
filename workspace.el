(require 'ivy)

;; TODO: This needs to be organized its a little unkempt

(defvar *workspaces* '())
(defvar *current-workspace* nil)

(defun workspace--add-ivy-view (view-name)
  ;;TODO: This should be named add-or-update...
  (let ((view (ivy--get-view-config))
	(x (assoc view-name ivy-views)))
    (if x
        (setcdr x (list view))
      (push (list view-name view) ivy-views))))

(defun workspace--set-workspace-and-switch! (ws-name)
  (setq
   *current-workspace* (workspace-key-from-name ws-name))
  (ivy--switch-buffer-action ws-name))

(defun workspace-switch-workspace (ws-name)
  (if (not *current-workspace*)
      (workspace--set-workspace-and-switch! ws-name)
    (let ((current-ws-name
	   (cdr (assoc *current-workspace* *workspaces*))))
      (if (equal ws-name current-ws-name)
	  (ivy--switch-buffer-action current-ws-name)
	(progn
	  (workspace--add-ivy-view current-ws-name)
	  (workspace--set-workspace-and-switch!
	   ws-name))))))

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

(defun mode-line-workspace ()
  (let ((result ""))
    (dolist (key (workspace-list-workspace-keys))
      (if (equal key *current-workspace*)
	  (setq
	   result (concat
		   result (format "<%s>" key)))
	(setq
	 result (concat
		 result (format "[%s]" key)))))
    result))

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
    
	
