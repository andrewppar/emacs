;;; todoist.el --- Todoist API -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify ;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Elementary Todoist API

;;; Code:

(require 'request)

(setq todoist-access-token "b5c3c9309168dc63350ef757b41b84c1de324d6e")

;;; Utils
(defun todoist--day-of-week (date)
  (let ((day-num (calendar-day-of-week date)))
    (cl-case day-num
      (0 "Sun")
      (1 "Mon")
      (2 "Tue")
      (3 "Wed")
      (4 "Thu")
      (5 "Fri")
      (6 "Sat"))))

(defun todoist--date-string-to-date (date-string)
  (let* ((year-mon-day (split-string date-string "-"))
	 (year         (string-to-number (car year-mon-day)))
	 (month        (string-to-number (cadr year-mon-day)))
	 (day          (string-to-number (caddr year-mon-day))))
    (list month day year)))

;;; Tasks

(defun todoist--maybe-nullify (string)
  (if string
      string
    "null"))

(defun todoist-request (postfix type)
  (let* ((auth-bearer (format "Bearer %s" todoist-access-token))
	 (headers `(("Authorization" . ,auth-bearer)))
	 (url      (format "https://api.todoist.com/rest/v2/%s" postfix)))
    (-> (request url :type type :sync t :headers headers)
	request-response-data
	todoist--maybe-nullify
	json-parse-string)))

(defun todoist-get-tasks ()
  (todoist-request "tasks" "GET"))

(defun todoist-close-task (task-id)
  (let* ((postfix (format "tasks/%s/close" task-id)))
    (todoist-request postfix "POST")))

(defun todoist-active-task-p (task-hash)
  (equal (gethash "is_completed" task-hash) :false))

(defun todoist--get-due (due-hash)
  (let ((due-date nil)
	(due-time nil))
    (if-let ((datetime (gethash "datetime" due-hash)))
	(setq due-date (-> datetime (split-string "T") car)
	      due-time (-> datetime (split-string "T") cadr))
      (setq due-date (gethash "date" due-hash)))
    (let* ((due-day (-> due-date
			todoist--date-string-to-date
			todoist--day-of-week))
	   (result (format "<%s %s" due-date due-day)))
      (if due-time
	  (setq result (format "%s %s>" result due-time))
	(setq result (format "%s>" result))))))

(defun todoist-serialize-task (task-hash indent)
  (let* ((id       (gethash "id" task-hash))
	 (content  (gethash "content" task-hash))
	 (due-spec (todoist--get-due (gethash "due" task-hash)))
	 (prefix   (make-string indent ?\*)))
    (format "%s TODO %s\nSCHEDULED: %s\n:PROPERTIES:\n:id: %s\n:END:\n"
	    prefix content due-spec id)))

(defun todoist-serialize-tasks (tasks indent)
  (let ((result ""))
    (doarray task tasks
      (when (todoist-active-task-p task)
	(setq result
	      (format "%s%s"
		      result (todoist-serialize-task task indent)))))
    result))

;;; todoist.el ends here
