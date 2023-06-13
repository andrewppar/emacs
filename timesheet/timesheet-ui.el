;;; timesheet-ui.el --- The (Emacs) UI -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 31 March 2023
;; Homepage: N/A
;; Keywords: gst
;; Package-Requires: ((emacs "28"))
;; SPDX-License-Identifier: MIT
;; Version: 3.0

;;; Commentary:

;; Provide functions for filling buffers with timesheet related
;; information

;;; Code:

;; I don't want to make a tar and go through the whole packaging process right now
(require 'timesheet-tasks)
(require 'widget)

(defmacro finsert (format-string &rest args)
  "Insert FORMAT-STRING with ARGS to current buffer at point."
  (declare (indent 0))
  `(insert (format ,format-string ,@args)))

(defvar *task-card-window-layout* nil)

(defun card-line-value (line-type)
  "Get the value for LINE-TYPE in a task card buffer."
  (let ((line-num (cl-case line-type
		    (:start-time  3)
		    (:end-time    4)
		    (:group       5)
		    (:description 6))))
    (save-excursion
      (goto-line line-num)
      (let ((line (thing-at-point 'line t)))
	(-> (thing-at-point 'line t)
	    (split-string ":")
	    cdr
	    (string-join ":")
	    string-trim)))))

(defun send-task-information ()
  "Send the task-card buffer information to the task database."
  (interactive)
  (let ((start (card-line-value :start-time))
	(end   (card-line-value :end-time))
	(group (card-line-value :group))
	(desc  (card-line-value :description))
	(date  (format-time-string "%m-%d-%Y")))
    (cond ((not (and start end date group desc))
	   (message "Some required fields missing"))
	  ((not (timesheet-valid-group? group))
	   (message "%s is not a valid timesheet group" group))
	  (t (progn
	       (timesheet-add! date start end group desc)
	       (kill-buffer)
	       (todays-task-log)
	       (set-window-configuration *task-card-window-layout*)
	       (setq *task-card-window-layout* nil))))))

(define-minor-mode task-add-card-mode
    "Minor mode for reviewing timesheet tasks."
  :init-value nil)
(evil-define-minor-mode-key 'normal 'task-add-card-mode
  (kbd "C-c C-c") 'send-task-information)

(defun log-task! (time-range)
  "Log a task to the database.

   TIME-RANGE specifies the start and end of the work on task."
  (interactive "sTime: ")
  (ensure-groups-valid)
  (let (start-time end-time date description issue)
    (cl-multiple-value-bind (parsed-start-time parsed-end-time)
	(parse-time-range time-range)
      (setq start-time parsed-start-time
	    end-time   parsed-end-time
	    date       (format-time-string "%m-%d-%Y")
	    issue      ""
	    group      (ido-completing-read
			"Select Group: " timesheet-groups nil t))
      (when (equal group "Conure")
	(setq issue (read-string "GitHub Issue: ")))
      (setq description (read-string "Task Description: "))
      (unless (equal issue "")
	(setq description (format "%s: %s" issue description))))
    (let ((edit?  (y-or-n-p "Would you like to review these entries?"))
	  (layout (current-window-configuration)))
      (if edit?
	  (let* ((height       (- (frame-height) 15))
		 (card-window (split-window (frame-root-window) height)))
	    (select-window card-window)
	    (setq *task-card-window-layout* layout)
	    (let ((card-buffer "*log-task*"))
	      (switch-to-buffer card-buffer)
	      (task-add-card-mode)
	      (insert "Log Task\n")
	      (insert "======================\n")
	      (finsert "Start Time: %s\n" start-time)
	      (finsert "End Time: %s\n" end-time)
	      (finsert "Group: %s\n" group)
	      (finsert "Description: %s\n" description)))
	(timesheet-add! date start-time end-time group description)
	(todays-task-log)
	(set-window-configuration layout)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generating Task Buffer ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-open-task-log (log-name &rest body)
  "Open a read only buffer with LOG-NAME to execute the contents of BODY in."
  (declare (indent defun))
  `(progn
     (switch-to-buffer ,log-name)
     (when buffer-read-only
       (setq buffer-read-only nil))
     (kill-region (point-min) (point-max))
     (goto-char 0)
     ,@body
     ;; everything is org? Maybe I'll change my mind on this
     (unless (equal major-mode 'org-mode)
       (org-mode))
     (read-only-mode)))

(defun insert-problem-table (problems)
  "Turn an alist of PROBLEMS into an 'org-mode' table."
  (let ((divider "|--|--|\n"))
    (save-excursion
      (insert divider)
      (insert "| Start Time | End Time |\n")
      (insert divider)
      (dolist (problem problems)
	(cl-destructuring-bind (start . end)
	    problem
	  (finsert "|%s|%s|\n" start end)))
      (insert divider)
      (forward-line -1)
      (org-table-align))))

(defun log-task-insert-properties (props)
  "Insert org properties into a log file.

  PROPS is an alist of property symbols (to be capitalized) and
  values for them."
  (insert ":PROPERTIES:\n")
  (dolist (property props)
    (cl-destructuring-bind (property . value)
	property
      (let ((property-string (-> property symbol-name upcase)))
	(finsert ":%s: %s\n" property-string value))))
  (insert ":END:\n"))

(defun serialize-group-to-buffer (group tasks depth date)
  "Add a GROUP of TASKS to an org buffer.

  DEPTH indicates what 'org-mode' level the group should be inserted at.
  'org-mode' level translates to the number of prefix stars."
  (dolist (task tasks)
    (let* ((desc   (alist-get 'description task))
	   (start  (alist-get 'start task))
	   (end    (alist-get 'end task))
	   (prefix (make-string depth ?*))
	   (duration (calculate-duration-string start end)))
      (finsert "%s %s--%s: %s\n" prefix start end desc)
      (log-task-insert-properties
       `((start . ,start)
	 (end . ,end)
	 (duration . ,duration)
	 (group . ,group)
	 (date . ,date))))))

(defun serialize-tasks-to-buffer
    (tasks depth structure show-problems? date)
  "Given a list of TASKS organized by group add them to current bufer.

   STRUCTURE specifies how tasks are grouped, e.g. by date or group.
   DEPTH indicates at what level the groups occur in the context
   of the buffer.
   If SHOW-PROBLEMS? is true then any disontinuity in the time is be displayed."
  (let* ((total-time (sum-tasks-time tasks))
	 ;; TODO: This needs to print leading 0s for numbers smaller than 10
	 (total-string (format "%s:%s" (car total-time) (cdr total-time)))
	 (gaps       (when show-problems? (get-time-gaps tasks)))
	 (prefix     (make-string depth ?*)))
    (when (equal depth 1)
      (pcase structure
	(:group (insert "#+STARTUP: show2levels\n"))
	(:date  (insert  "#+STARTUP: show3levels\n"))))
    (let ((grouped-tasks (group-tasks-by tasks structure)))
      (doalist (group group-tasks grouped-tasks)
	(let ((group-time (sum-tasks-time group-tasks)))
	  (finsert "%s %s %s:%s\n"
		   prefix group (car group-time) (cdr group-time))
	  (cond ((equal structure :group)
		 (serialize-group-to-buffer
		  group group-tasks (1+ depth) date))
		((equal structure :date)
		 (serialize-tasks-to-buffer
		  group-tasks (1+ depth) :group nil group))
		(t
		 (error "Unrecognized serialization structure"))))))
    (finsert "%s TOTAL TIME: %s\n" prefix total-string)
    (when (and show-problems? gaps)
      (insert "* GAPS \n")
      (insert-problem-table gaps))))

(defun task-log-view-log (buffer-name tasks structure date)
  "Create a buffer for TASKS with BUFFER-NAME.

   STRUCTURE indicates how the tasks should be structured for
   serialization."
  (let ((show-problems? (cl-case structure
			  (:group t)
			  (:date  nil))))
    (with-open-task-log buffer-name
	(serialize-tasks-to-buffer tasks 1 structure show-problems? date))))

(defun task-log-internal (&optional start-date end-date)
  "Log tasks for optional START-DATE and END-DATE.

  If no date is specified then today is used.
  If only START-DATE is specified that is treated as END-DATE.
  If only END-DATE is specified an error is thrown."
  (when (and (not start-date) end-date)
    (error "Cannot specify end date without start date"))
  (let* ((today       (format-time-string "%m-%d-%Y"))
	 (start       (or start-date today))
	 (end         (or end-date start))
	 (buffer-name (cond ((equal start today)
			     "*todays-task-log*")
			    ((equal start end)
			     (format "*task-log-%s" start))
			    (t
			     (format "*task-log-%s-%s" start end))))
	 (buffer    (switch-to-buffer buffer-name))
	 (tasks     (get-tasks-for-dates start end))
	 (view-type (if (equal start end) :group :date))
	 (date      (when (equal view-type :group) start)))
    (task-log-view-log buffer tasks view-type date)))

(defun todays-task-log ()
  "Task log for today."
  (interactive)
  (task-log-internal)
  (hide-sublevels 2))

(defun task-log-for-date (date)
  "Task log for DATE."
  (interactive "sDate: ")
  (task-log-internal date)
  (hide-sublevels 2))

(defun timesheet-get-date (prompt)
  "Get a date from user with PROMPT."
    (->> prompt
	(org-read-date nil 'to-time nil)
	(format-time-string "%m-%d-%Y")))

(defun task-log-for-dates ()
  "Create task log for date range specified by prompting user."
  (interactive)
  (let ((start (timesheet-get-date "Start Date: "))
	(end   (timesheet-get-date "End Date: ")))
    (task-log-internal start end)
    (hide-sublevels 3)))

;; Operating on Task Buffer

(defmacro plist-get-in (plist keys)
  "Like clojure's get-in - works on PLIST with list of KEYS."
  (declare (indent 0))
  (if (length= keys 1)
      `(plist-get ,plist ,(car keys))
    `(plist-get-in (plist-get ,plist ,(car keys)) ,(cdr keys))))

(defun timesheet--get-entry-at-point ()
  "Get properties of the timesheet entry at the point."
  (let ((at-entry?    nil))
    (save-excursion
      (forward-line)
      (setq at-entry? (org-at-property-drawer-p)))
    (if at-entry?
	(let* ((properties  (org-entry-properties))
	       (description (-> "ITEM"
				(alist-get properties nil nil #'equal)
				(split-string ":")
				cdddr
				(string-join ":"))))
	  (make-timesheet-task
	   :date (alist-get "DATE" properties nil nil #'equal)
	   :start (alist-get "START" properties nil nil #'equal)
	   :end (alist-get "END" properties nil nil #'equal)
	   :group (alist-get "GROUP" properties nil nil #'equal)
	   :description description))
      (message "No task at point"))))

(defun timesheet-delete-task-at-point ()
  "Delete the task at point."
  (interactive)
  (let ((task (timesheet--get-entry-at-point)))
    (cl-multiple-value-bind (date start end group description)
	(timesheet-destructure-task task)
      (timesheet-delete! date start end))))

(defun timesheet-edit-internal (slot new-val)
  "Update the task at point for :SLOT with NEW-VAL."
  (let ((task (timesheet--get-entry-at-point)))
    (when task
      (timesheet-edit! task slot new-val))))

(defun timesheet-edit-description-at-point (new-description)
  "Update the description of the entry at point with NEW-DESCRIPTION."
  (interactive "sNew Description: ")
  (timesheet-edit-internal :description new-description))

(defun timesheet-edit-end-time-at-point (new-end)
  "Update the end time of the entry at point with NEW-END."
  (interactive "sNew End Time: ")
  (timesheet-edit-internal :end new-end))

(defun timesheet-edit-group-at-point ()
  "Update the group of the entry at point with NEW-GROUP."
  (interactive)
  (let ((new-group (ido-completing-read
		"Select Group: " timesheet-groups)))
    (timesheet-edit-internal :group new-group)))

(defun timesheet-edit-start-time-at-point (new-start)
  "Update the start time of the entry at point with NEW-START."
  (interactive "sNew Start Time: ")
  (timesheet-edit-internal :start new-start))








(comment "good luck!"
	 (aref (aref (aref array 1) 2 ) 3)

	 (-> array
	     (aref 1)
	     (aref 2)
	     (aref 3))


	 (defun sexp-thread-first-internal (sexp result)
	   (let (())))

	 (defun sexp-thread-first (sexp)
	   (let ((result '(->)))
	     (sexp-thread-first-internal sexp result)))


	 (defun thread-first ()
	   (interactive)
	   (let ((start (point))
		 (end   nil))
	     (save-excursion
	       (forward-sexp)
	       (setq end (point)))
	     (let* ((sexp (read (buffer-substring start end)))
		    (result (format "%s" (sexp-thread-first sexp))))
	       (kill-sexp)
	       (insert result)))))

(provide 'timesheet-ui)
;;; timesheet-ui.el ends here
