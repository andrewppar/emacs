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

(defmacro -> (item &rest forms)
  (cond ((not forms)
	 item)
	((length= forms 1)
	 (let ((form (car forms)))
	   (if (listp form)
	       `(,(car form) ,item ,@(cdr form))
	     (list form item))))
	(t
	 `(->
	   (-> ,item ,(car forms))
	   ,@(cdr forms)))))

(defmacro ->> (item &rest forms)
  (cond ((not forms)
	 item)
	((length= forms 1)
	 (let ((form (car forms)))
	   (if (listp form)
	       (reverse
		(cons item (reverse form)))
	     (list form item))))
	(t
	 `(->>
	   (->> ,item ,(car forms))
	   ,@(cdr forms)))))

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
  (let ((workspaces-set? *workspaces*))
  (if (workspace--workspace-name-exists ws-name)
      (message
       (format "Workspace with name %s already exists" ws-name))
    (progn
      (setq *current-workspace* number)
      (push (cons number ws-name) *workspaces*)
      (workspace--add-ivy-view ws-name)
      (unless workspaces-set?
	(add-hook 'change-major-mode-hook 'workspace-new-buffer))))))

(defun workspace--get-next-workspace-number-internal
    (workspace-numbers)
  (let ((sorted-numbers (sort workspace-numbers #'<)))
    (cl-do ((last-number    0 current-number)
	    (current-number (car sorted-numbers) (car todo))
	    (todo           (cdr sorted-numbers) (cdr todo)))
	((or (not todo)
	     (not (equal (inc last-number) current-number)))
	 (if (not (equal (inc last-number) current-number))
	     (inc last-number)
	   (inc (inc last-number)))))))

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

(defun workspace-to-workspace-number (n)
  (let ((workspaces-set? *workspaces*))
    (when (<= n 0)
      (error "Workspace number must be at least 1."))
    (if-let ((ws-name (cdr (assoc n *workspaces*))))
	(workspace-switch-workspace ws-name)
      (workspace-add-workspace n))
    (unless workspaces-set?
      (add-hook 'buffer-list-update-hook 'workspace-new-buffer))))

(defun workspace--remove-workspace (n)
  (setq *workspaces*
	(assoc-delete-all n *workspaces*))
  (setq *workspace-workspace-buffers*
	(assoc-delete-all n *workspace-workspace-buffers*)))

(defun workspace-remove-workspace (ws-name)
  (let ((ws-number  (-> ws-name
			(split-string ":")
			car
			string-to-number)))
    (if (equal ws-number *current-workspace*)
	(message "Cannot delete the current workspace")
      (let ((buffers (alist-get
		      ws-number *workspace-workspace-buffers*)))
	(workspace--remove-workspace ws-number)
	(ivy-pop-view-action (assoc ws-name ivy-views))
	(dolist (buffer buffers)
	  (kill-buffer buffer))))))

(defun workspace-pop ()
  (interactive)
  (let* ((workspaces (workspace-list))
	 (to-remove  (ivy-read "Pop Workspace: " workspaces)))
    (workspace-remove-workspace to-remove)))


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
;;; workspace.el ends here
