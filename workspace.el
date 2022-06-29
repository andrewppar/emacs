(require 'ivy)

(defvar *workspaces* '())
(defvar *current-workspace* nil)
(defvar *workspace-workspace-buffers* '())

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
	  (workspace--set-workspace-and-switch! ws-name))))))

(defun workspace-list-workspace-names ()
  (let ((result '()))
    (dolist (entry *workspaces*)
      (push (cdr entry) result))
    result))

(defun workspace-list ()
  (let ((result '()))
    (dolist (entry *workspaces*)
      (let ((idx     (car entry))
	    (ws-name (-> entry cdr (substring 3))))
	(push
	 (format "%s: %s" idx ws-name) result)))
    (sort result 'string-lessp)))

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
  (let ((result "")
	(keys (sort (workspace-list-workspace-keys) '<=)))
    (dolist (key keys)
      (if (equal key *current-workspace*)
	  (setq
	   result
	   (->> 3
	     (substring (alist-get key *workspaces*))
	     (format "<%s>")
	     (concat result)))
	(setq
	 result (concat
		 result (format "[%s]" key)))))
    result))

(defun workspace--workspace-name-exists (ws-name)
  (member ws-name (workspace-list-workspace-names)))

(defun workspace--add-workspace-no-prompt (number ws-name)
  ;; NOTE: This function is not safe and needs some
  ;; guard rails since I want to be able to call it from
  ;; organizer-session
  (if (workspace--workspace-name-exists ws-name)
      (message
       (format "Workspace with name %s already exists" ws-name))
    (progn
      (setq *current-workspace* number)
      (push (cons number ws-name) *workspaces*)
      (workspace--add-ivy-view ws-name))))


(defun workspace-add-workspace (n)
  (let ((name
	 (ivy-read
	  "Name workspace: "
	  nil
	  :initial-input "{} "
	  ;; (ivy-default-view-name)
	  )))
    (workspace--add-workspace-no-prompt n name)))

(defun workspace-to-workspace-number (n)
  (when (<= n 0)
    (error "Workspace number must be at least 1."))
  (if-let ((ws-name (cdr (assoc n *workspaces*))))
      (workspace-switch-workspace ws-name)
    (workspace-add-workspace n))
  (unless *workspaces*
    (add-hook 'buffer-list-update-hook 'workspace-new-buffer)))

(defun workspace--remove-workspace (n)
  (setq *workspaces*
	(assoc-delete-all n *workspaces*)))

(defun workspace-pop ()
  (interactive)
  (let* ((workspaces (workspace-list))
	 (to-remove  (ivy-read
		      "Pop Workspace: "
		      workspaces))
	 (ws-number  (-> to-remove
			 (split-string ":")
			 car
			 string-to-number)))
    (if (equal ws-number *current-workspace*)
	(message "Cannot delete the current workspace")
      (progn
	(workspace--remove-workspace ws-number)
	(ivy-pop-view-action (assoc to-remove ivy-views))))))

(defun workspace-new-buffer ()
  (if (not *current-workspace*)
      (message "Cannot use store workspace if no workspace is set")
    (let* ((buffer   (current-buffer))
	   (raw-workspace-buffers (delete-dups
			       (alist-get
				*current-workspace*
				*workspace-workspace-buffers*)))
	   (workspace-buffers '()))
      (dolist (buffer raw-workspace-buffers)
	(when (buffer-live-p buffer)
	  (push buffer workspace-buffers)))

      (setq *workspace-workspace-buffers*
	    (assoc-delete-all
	     *current-workspace* *workspace-workspace-buffers*))
      (push (cons *current-workspace* (cons buffer workspace-buffers))
	    *workspace-workspace-buffers*))))

(defun workspace-switch-buffer ()
  (interactive)
  (ivy-read "Switch to buffer: "
	    (mapcar
	     #'buffer-name
	     (alist-get *current-workspace* *workspace-workspace-buffers*))
            :keymap ivy-switch-buffer-map
            :preselect (buffer-name (other-buffer (current-buffer)))
            :action #'ivy--switch-buffer-action
            :matcher #'ivy--switch-buffer-matcher
            :caller 'ivy-switch-buffer))
