;;; workspace.el --- Provide Eirene Workspaces -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
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

;; Create, Manage, and Delete Eirene Workspaces

;;; Code:
(require 'ivy)

(defvar *workspaces* '())
(defvar *current-workspace* nil)
(defvar *workspace-workspace-buffers* '())

(defun current-workspace ()
  *current-workspace*)

(defun length= (lista num)
  (equal (length lista) num))

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

(defun workspace--add-or-update-ivy-view (view-name)
  (let ((view (ivy--get-view-config))
	(x (assoc view-name ivy-views)))
    (if x
        (setcdr x (list view))
      (push (list view-name view) ivy-views))))

(defun worskpace-save-current-view ()
  (if (not *current-workspace*)
      (message "There is no workspace to save")
    (let* ((current-view    (ivy--get-view-config))
	   (current-ws-name (cdr
			     (assoc *current-workspace* *workspaces*)))
	   (old-config      (assoc current-ws-name ivy-views)))
      (setcdr old-config (list current-view)))))

(defun workspace--set-workspace-and-switch! (ws-name)
  (setq *current-workspace* (workspace-key-from-name ws-name))
  (ivy--switch-buffer-action ws-name))

(defun workspace-switch-workspace (ws-name)
  (if (not *current-workspace*)
      (workspace--set-workspace-and-switch! ws-name)
    (let ((current-ws-name (cdr
			    (assoc *current-workspace* *workspaces*))))
      (if (equal ws-name current-ws-name)
	  (ivy--switch-buffer-action current-ws-name)
	(progn
	  (workspace--add-or-update-ivy-view current-ws-name)
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
  (let ((workspaces-set? *workspaces*))
    (if (workspace--workspace-name-exists ws-name)
	(message
	 (format "Workspace with name %s already exists" ws-name))
      (let ((buffer (get-buffer "*scratch*")))
	(setq *current-workspace* number)
	(push (cons number ws-name) *workspaces*)
	(delete-other-windows)
	(switch-to-buffer buffer)
	(workspace--add-or-update-ivy-view ws-name)
	(push (cons *current-workspace* '())
	      *workspace-workspace-buffers*)))
    (unless workspaces-set?
      (add-hook 'change-major-mode-hook 'workspace-new-buffer))))

(defun workspace--get-next-workspace-number-internal
    (workspace-numbers)
  (let ((sorted-numbers (sort workspace-numbers #'<)))
    (cl-do ((last-number    0 current-number)
	    (current-number (car sorted-numbers) (car todo))
	    (todo           (cdr sorted-numbers) (cdr todo)))
	((or (not todo)
	     (not (equal (+ 1 last-number) current-number)))
	 (if (not (equal (+ 1 last-number) current-number))
	     (+ 1 last-number)
	   (+ 1 (+ 1 last-number)))))))

(defun workspace-get-next-workspace-number ()
  (let ((workspace-numbers (workspace-list-workspace-keys)))
    (if workspace-numbers
	(workspace--get-next-workspace-number-internal
	 workspace-numbers)
      1)))

(defun workspace-add-workspace (n)
  (let ((name
	 (ivy-read
	  "Name workspace: "
	  nil
	  :initial-input "{} "
	  ;; (ivy-default-view-name)
	  )))
    (workspace--add-workspace-no-prompt n name)))

(defun workspace--to-workspace-number (n name)
  (when (<= n 0)
    (error "Workspace number must be at least 1"))
  (if-let ((ws-name (cdr (assoc n *workspaces*))))
      (workspace-switch-workspace ws-name)
    (if name
	(workspace--add-workspace-no-prompt n name)
      (workspace-add-workspace n))))

(defun workspace-to-workspace-number (n)
  (interactive)
  (workspace--to-workspace-number n nil))

(defun workspace-to-workspace-number-with-name (n name)
  (interactive)
  (workspace--to-workspace-number n name))

(defun workspace--remove-workspace (n)
  (setq *workspaces*
	(assoc-delete-all n *workspaces*))
  (setq *workspace-workspace-buffers*
	(assoc-delete-all n *workspace-workspace-buffers*)))

(defun workspace-remove-workspace-number (ws-number)
  (let ((buffers (alist-get ws-number *workspace-workspace-buffers*))
	(ws-name (car (assoc ws-number *workspaces*))))
    (workspace--remove-workspace ws-number)
    (ivy-pop-view-action (assoc ws-name ivy-views))
    (dolist (buffer buffers)
      (kill-buffer buffer))
    ;; Only do this if the on ebeing removed is the current one
    (let ((next-workspace (caar *workspaces*)))
      (if next-workspace
	  (workspace-to-workspace-number (caar *workspaces*))
	(setq *current-workspace* nil)))))

(defun workspace-remove-workspace (ws-name)
  (let ((ws-number  (-> ws-name
			(split-string ":") car string-to-number)))
    (workspace-remove-workspace-number ws-number)))

(defun workspace-pop ()
  (interactive)
  (let* ((workspaces (workspace-list))
	 (to-remove  (ivy-read "Pop Workspace: " workspaces)))
    (workspace-remove-workspace to-remove)))

(defun workspace--live-buffers (buffers)
  (filter (lambda (buffer) (buffer-live-p buffer)) buffers))

(defun workspace-new-buffer ()
  (if (not *current-workspace*)
      (message
       "Cannot use workspace new buffer workspace if no workspace is set")
    (let* ((buffer   (current-buffer))
	   (raw-workspace-buffers (delete-dups
				   (alist-get
				    *current-workspace*
				    *workspace-workspace-buffers*)))
	   (workspace-buffers (workspace--live-buffers
			       raw-workspace-buffers)))
      (setq *workspace-workspace-buffers*
	    (assoc-delete-all
	     *current-workspace* *workspace-workspace-buffers*))
      (push (cons *current-workspace* (cons buffer workspace-buffers))
	    *workspace-workspace-buffers*))))

(defun clean-workspace-buffers ()
  (let ((to-save '())
	(numbers (mapcar #'car *workspaces*)))
    (dolist (workspace-number numbers)
      (let ((buffers (alist-get
		      workspace-number *workspace-workspace-buffers*)))
	(dolist (buffer (workspace--live-buffers buffers))
	  (push buffer to-save))))
    (let ((old-numbers (mapcar #'car *workspace-workspace-buffers*)))
      (dolist (number old-numbers)
	(let* ((all-buffers
		(alist-get number *workspace-workspace-buffers*))
	       (live-buffers (workspace--live-buffers all-buffers)))
	  (setq *workspace-workspace-buffers*
		(assoc-delete-all number *workspace-workspace-buffers*))
	  (if (alist-get number *workspaces*)
	      (push (cons number live-buffers) *workspace-workspace-buffers*)
	    (dolist (buffer live-buffers)
	      (unless (member buffer to-save)
		(kill-buffer buffer)))))))
    (ivy-pop-view-action (assoc nil ivy-views))))



(defun workspace-switch-buffer ()
  (interactive)
  (when *workspace-workspace-buffers*
    (clean-workspace-buffers))
  (if  *workspace-workspace-buffers*
      (ivy-read "Switch to buffer: "
		(mapcar
		 #'buffer-name
		 (alist-get *current-workspace* *workspace-workspace-buffers*))
		:keymap ivy-switch-buffer-map
		:preselect (buffer-name (other-buffer (current-buffer)))
		:action #'ivy--switch-buffer-action
		:matcher #'ivy--switch-buffer-matcher
		:caller 'ivy-switch-buffer)
    (counsel-switch-buffer)))

(defmacro save-workspace-excursion (ws-name &rest body)
  (declare (indent 1))
  (let ((current-ws (gensym)))
    `(let ((,current-ws (current-workspace)))
       (workspace-switch-workspace (format "{} %s" ,ws-name))
       ,@body
       (workspace-to-workspace-number ,current-ws))))

(defmacro save-layout-excursion (&rest body)
  "Execute BODY restoring the previous window layout when finished."
  (let ((current-layout (gensym)))
    `(let ((,current-layout (current-window-configuration)))
       (progn ,@body)
       (set-window-configuration ,current-layout))))



;;; workspace.el ends here
