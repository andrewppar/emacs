;;; timesheet-taks.el --- Process Tasks -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 31 March 2023
;; Homepage: N/A
;; Keywords: gst
;; Package-Requires: ((emacs "28"))
;; SPDX-License-Identifier: MIT
;; Version: 3.0

;;; Commentary:

;; Provide functionality for organizing data that is retrieved from
;; the timesheet endpoint.

;;; Code:
(require 'request)

(defvar timesheet-server "192.168.1.139")
(defvar timesheet-port 3000)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data Engineering (っ▀¯▀)つ ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Time

(defun parse-time (time-string)
  "Parse a TIME-STRING of the from HH:MM."
  (-> time-string
      parse-time-string
      decoded-time-set-defaults
      encode-time))

(defun parse-time-range (time-range-string)
  "Split a TIME-RANGE-STRING into it's start and end."
  (let ((split-times (split-string (string-trim time-range-string) "-")))
    (cl-values (string-trim (car split-times))
	       (string-trim (car (last split-times))))))

(defun sum-tasks-time (tasks)
  "Given a list of TASKS get the total time it took to complete them."
  (unless (vectorp tasks)
    (setq tasks (apply #'vector tasks)))
  (let* ((result 0)
	 (seconds-per-minute 60))
    (doarray (task tasks)
      (let ((start (alist-get 'start task))
	    (end   (alist-get 'end task)))
	(setq result (+ result
			(-> (parse-time end)
			    (time-subtract (parse-time start))
			    (/ seconds-per-minute))))))
    (let* ((hours (/ result 60))
	   (minutes (- result (* hours 60))))
      `(,hours . ,minutes))))

(defun make-time-alist (tasks)
  "Make start-time end-time alist for TASKS.

  Given a list of TASKS  create an alist
  binding the start time of each task to its end time."
  (let ((result '()))
    (doarray (task tasks)
      (let ((start (alist-get 'start task))
	    (end   (alist-get 'end task)))
	(push (cons start end) result)))
    result))

(defun get-time-gaps (tasks)
  "Get timeline gaps in TASKS.

   Given a list of TASKS find any gaps in the timeline
   for those tasks."
  (let* ((unmatched-starts '())
	 (unmatched-ends   '())
	 (time-alist       (make-time-alist tasks))
	 (ends             (mapcar #'cdr time-alist)))
    (dolist (time-binding time-alist)
      (cl-destructuring-bind (start . end)
	  time-binding
	(unless (member start ends)
	  (push start unmatched-starts))
	(unless (alist-get end time-alist nil nil #'equal)
	  (push end unmatched-ends))))
    (let* ((sorted-starts (sort unmatched-starts #'string<))
	   (sorted-ends   (sort unmatched-ends #'string<))
	   (gap-starts    (cdr sorted-starts))
	   (gap-ends      (-> sorted-ends reverse cdr reverse))
	   (done?         nil)
	   (result        '()))
      (when (or gap-starts gap-ends)
	(while (not done?)
	  (let ((block-end       (car gap-ends))
		(new-block-start (car gap-starts)))
	    (push (cons block-end new-block-start) result)
	    (setq gap-starts (cdr gap-starts))
	    (setq gap-ends   (cdr gap-ends))
	    (setq done? (not (or  gap-starts gap-ends))))))
      (reverse result))))

(defun calculate-duration-string (start-time end-time)
  "Display the difference between START-TIME and END-TIME."
  (let* ((start    (parse-time start-time))
	 (end      (parse-time end-time))
	 (duration (/ (time-subtract end start) 60))
	 (hours    (/ duration 60))
	 (minutes  (- duration (* hours 60))))
    (format "%s:%s" hours minutes)))

;;; Task Changes

(defun group-tasks-by (tasks structure)
  "Group TASKS according to STRUCTURE."
  (unless (vectorp tasks)
    (setq tasks (apply #'vector tasks)))
  (let ((result '())
	(key    (-> structure symbol-name (substring 1) intern)))
    (doarray (task tasks)
      (let ((group (alist-get key task)))
	(setq result
	      (alist-update result group #'append (list task)))))
    result))

(cl-defstruct timesheet-task
  (date
   nil
   :type stringp
   :documentation "The date associated with the task")
  (start
   nil
   :type stringp
   :documentation "The start time of the task")
  (end
   nil
   :type stringp
   :documentation "The end time of the task")
  (group
   nil
   :type stringp
   :documentation "The group the task was done for")
  (description
   nil
   :type stringp
   :documentation "The task description"))

(defun timesheet-destructure-task (task)
  "Destructure TASK."
  (let ((date        (timesheet-task-date task))
	(start       (timesheet-task-start task))
	(end         (timesheet-task-end task))
	(group       (timesheet-task-group task))
	(description (timesheet-task-description task)))
    (cl-values date start end group description)))

;;;;;;;;;;;;;;;;;;
;;; Data Fetch ;;;
;;;;;;;;;;;;;;;;;;

(defun run-timesheet-command (postfix data post?)
  "Send an http request with DATA to the timesheet server at POSTFIX."
  (let* ((base      (format "%s:%s" timesheet-server timesheet-port))
	 (url       (if postfix (format "%s/%s" base postfix) base))
	 (type      (if post? "POST" "GET"))
	 (headers   '(("Content-Type" . "application/json")))
	 (json-data (json-encode data)))
    (-> url
	(request :type type
	  :headers headers
	  :data json-data
	  :sync t
	  :parser 'json-read)
	request-response-data)))

(defun get-tasks-for-dates (start-date end-date)
  "Retrieve tasks between START-DATE and END-DATE."
  (run-timesheet-command
   "search" `(("start_date" . ,start-date)
	      ("end_date" . ,end-date))
   t))

(defun get-tasks-for-date (date)
  "Get all tasks from DATE."
  (get-tasks-for-dates date date))

(defun get-todays-tasks ()
  "Get tasks for today."
  (let ((date (format-time-string "%m-%d-%Y")))
    (get-tasks-for-date date)))

(defmacro defcached (name args &rest body)
  (declare (indent defun))
  (let ((cache      (gensym))
	(hit?       (gensym))
	(result     (gensym))
	(cache-name (intern (format "%s-cache" name))))
    `(progn
       (defvar ,cache-name (make-hash-table))
       (defun ,name ,args
	 (if (gethash ',args ,cache-name)
	     (gethash ',args ,cache-name)
	   (let ((,result (progn ,@body)))
	     (puthash ',args ,result ,cache-name)
	     ,result))))))

(defcached task-get-valid-groups ()
  (let ((result '())
	(groups (run-timesheet-command "groups" nil nil)))
    (doarray (group groups)
      (push group result))
    result))

;;;;;;;;;;;;;;;;
;;; Data Add ;;;
;;;;;;;;;;;;;;;;

(defvar timesheet-groups
  '("Cisco"
    "Cisco - Comfort"
    "Cisco - Meeting"
    "Cisco - Onboarding"
    "Cisco - Pairing"
    "Conure"
    "Personal"
    "Klezmer Archives"))

(defun timesheet-valid-group? (group)
  "Check whether GROUP? is a valid group in the timesheet db."
  (member group (task-get-valid-groups)))


(defun ensure-groups-valid ()
  "Ensure that the TIMESHEET-GROUPS are all valid DB groups."
  (let ((db-groups (task-get-valid-groups)))
    (dolist (group timesheet-groups)
      (unless (member group db-groups)
	(throw :error
	  (format
	   "%s is not a group specified in the connected timesheet database" group))))))

(defun timesheet-add! (date start-time end-time group description)
  "Add a task to the database.

   Task is added for DATE, START-TIME, END-TIME, and GROUP, with
   DESCRIPTION."
  (run-timesheet-command
   "add"
   `(("date" . ,date)
     ("start_time" . ,start-time)
     ("end_time" . ,end-time)
     ("group" . ,group)
     ("description" . ,description))
   t))

(defun timesheet-delete! (date start end)
  "Delete task on DATE with START and END times."
  (run-timesheet-command "delete"
			 `(("date" . ,date)
			   ("start_time" . ,start)
			   ("end_time" . ,end))
			 t))

(defun timesheet-edit! (task slot value)
  "Update TASK in database on SLOT with VALUE."
  (cl-multiple-value-bind (date start end group description)
      (timesheet-destructure-task task)
    (timesheet-delete! date start end)
    (pcase slot
      (:start
       (setf (timesheet-task-start task) value))
      (:end
       (setf (timesheet-task-end task) value))
      (:date
       (setf (timesheet-task-date task) value))
      (:group
       (setf (timesheet-task-group task) value))
      (:description
       (setf (timesheet-task-description task) value)))
    (cl-multiple-value-bind
	  (new-date new-start new-end new-group new-description)
	(timesheet-destructure-task task)
      (run-timesheet-command
       "add"
       `(("date" . ,new-date)
	 ("start_time" . ,new-start)
	 ("end_time" . ,new-end)
	 ("group" . ,new-group)
	 ("description" . ,new-description))
       t))))

(provide 'timesheet-tasks)
;;; timesheet-tasks.el ends here
