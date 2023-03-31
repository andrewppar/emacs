;;; eirene-calendar.el --- Provide calendar function for Eirene -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Andrew Parisi

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

;; Sync calendar and Org

;;; Code:
(defvar schedule-file "/Users/andrewparisi/org/status.org")
(defvar schedule-header-path '("Schedule" "Once" "Today"))

;; Internal Org Mode Stuff

(defun header-text (string)
  "Get the text from a STRING thats an org mode header."
  (-> string
      string-trim
      split-string
      cadr
      string-trim))

(defmacro do-org-headers (header-var &rest body)
  (declare (indent defun))
  (let ((pos    (gensym)))
    `(save-excursion
       (goto-char 0)
       (cl-do ((,pos (re-search-forward "^\*" nil t)
		     (re-search-forward "^\*" nil t)))
	   ((not ,pos) nil)
	 (setq ,header-var (header-text (thing-at-point 'line)))
	 ,@body))))

(defun header-position (header-list)
  (let ((pos nil)
	(hl     header-list))
    (do-org-headers header
      (message header)
      (when (string-prefix-p header (car hl))
	(when (not (cdr hl))
	  (setq pos (point)))
	(setq hl (cdr hl))))
    pos))

;; Serialize calendar entries
(cl-defstruct calendar-entry
  "An entry to serialize into an org mode item."
  title start-time end-time day month year)

(cl-defgeneric calendar-entry-slots (ce depth)
  (let  ((org-stars  (make-string depth ?\*))
	 (title      (calendar-entry-title ce))
	 (start-time (calendar-entry-start-time ce))
	 (end-time   (calendar-entry-end-time ce))
	 (date       (list
		      (calendar-entry-month ce)
		      (calendar-entry-day ce)
		      (calendar-entry-year ce))))
    (cl-values org-stars title start-time end-time date)))

(cl-defgeneric serialize-entry (calendar-entry depth)
  "")

(cl-defstruct (diary-entry (:include calendar-entry)))


(cl-defmethod serialize-entry ((ce diary-entry) depth)
  "Serialize a CE as a DIARY-ENTRY with org-depth of DEPTH."
  (cl-multiple-value-bind (org-stars title start-time end-time date)
      (calendar-entry-slots ce date)
    (format "%s %s %s-%s\n<%%(equal date '%s)>"
	    org-stars title start-time end-time date)))


(cl-defstruct (org-todo (:include calendar-entry)))

(defun day-of-week (date)
  (let ((day-num (calendar-day-of-week date)))
    (cl-case day-num
      (0 "Sun")
      (1 "Mon")
      (2 "Tue")
      (3 "Wed")
      (4 "Thu")
      (5 "Fri")
      (6 "Sat"))))

(cl-defmethod serialize-entry ((entry org-todo) depth)
  (cl-multiple-value-bind (org-start title start-time end-time date)
      (calendar-entry-slots entry depth)
    (let* ((day-string (day-of-week date))
	   (date-string (format "%s-%s-%s"
				(caddr date)
				(car date)
				(cadr date)))
	   (stamp (format "SCHEDULED: <%s %s"
			  date-string
			  day-string)))
      (cond ((and start-time end-time)
	     (setq stamp (format "%s %s-%s>"
				 stamp start-time end-time)))
	    (start-time
	      (setq stamp (format "%s %s>"
				  stamp start-time)))
	    (t
	     (setq stamp (format "%s>" stamp))))
      (format "%s %s\n%s"
	      org-start title stamp))))


(cl-defstruct line-parser
  title-p-fn title-line-fn time-p-fn time-line-fn entries todo in-progress)

(defun parse-item (line-parser item type)
  "Parse a calendar item according to the passed type."
  )

(defun parse-line (line-parser line)
  "Parse a LINE of text for a calendar type with LINE-PARSER."
  (cond ((title-p-fn line)
	 (parse-item line-parser (title-line-fn line) :title))
	((time-p-fn line)
	 (parse-item line-parser (time-line-fn line)) :time)
	(t line-parser)))




  )

(cl-defstruct (ical-buddy-line-parser
	       (:include line-parser)
	       (:





(cl-defstruct calendar-line name validator parser)



(cl-defstruct (title-line (:include calendar-line)))





(defun create-line-parser (line)
  (cond ((title-line-p line)
	 (title-line :line line))
	((date-line-with-time-p line)
	 (date-line-with-time :line line))
	((date-line-p line)
	 (date-line :line line))
	((time-line-p line)
	 (time-line :line line))
	 (t
	  (calendar-line :line line))))





(cl-defgeneric parse-calendar-line ((line calendar-line))
  line)


(cl-defstruct (date-line (:include calendar-line)))


(cl-defmethod parse-calendar-line ((line date-line))
  (calendar-line-line line))





(cl-defgeneric parse-calendar-line (calendar-line)
  (let* ((line     (calendar-line-line calendar-line))
	 (fn       (calendar-line-parse-fn calendar-line))
	 (result   (apply fn `(,line))))
    (setf (calendar-line-result calendar-line) result)
    calendar-line))


(defclass date-line (calendar-line) nil)


(cl-defmethod parse-calendar-line ((line date-line))
(substring line 2))

(parse-calendar-line (new-date-line "DATE"))

(cl-defstruct date-line)


(cl-defstruct ical-buddy
  todo
  (current-item   nil)
  (calendar-items nil))

(defvar *ex* (make-ical-buddy :todo '("one" "two" "three")))

(cl-struct-slot-value 'ical-buddy 'todo *ex*)


(cl-defgeneric parse-line (parser line)
  (let* ((parse-type (type-of parser))
	 (todo (cl-struct-slot-value parse-type 'todo parser)))
    (if todo
	(let ((new-line (car todo))
	      (new-todo (cdr todo)))
	   (setf (cl-struct-slot-value parse-type 'todo parser)
		 new-todo)
	   (parse-line parser new-line))
      parser)))


(cl-defmethod parse-line ((parser ical-buddy) line))




(defun ical-buddy-parse-line (parser line)
  (cond ((title-line? line)
	 (ical-buddy-parse-title-line parser line))
	((date-and-time-line? line)
	 (ical-buddy-parse-date-and-time-line parser line))
	((date-line? line)
	 (ical-buddy-parse-date-line? parser line))
	((time-line? line)
	 (ical-buddy-parse-time-line? parser line))
	(t
	 (let ((todo     (ical-buddy-parser-todo parser))
	       (line     (car todo))
	       (new-todo (cdr todo)))

	   (ical-buddy-parse-line

;;; eirene-calendar.el ends here
