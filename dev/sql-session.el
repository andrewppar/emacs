;;; sql-session.el --- A SQL IDE                     -*- lexical-binding: t; -*-

   ;; Copyright (C) 2022  Andrew Parisi

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

   ;; A simple package to provide an IDE-like
   ;; experience when editing SQL queries

;;; Code:

(require 'sql)

(defvar *current-connection* nil)
(defvar *tables-buffer* "*tables*")
(defvar *sql-buffer*    "*sql*")
(defvar *sqli-connection-buffer* nil)
(defvar *current-tables* nil)
(defvar *sql-session-orientation* :horizontal)


(defun get-connection-information (connection)
  "Return the connection information for CONNECTION."
  ;; This is a VERY naive implementation of this
  ;; function
  (->> sql-connection-alist
       (assoc-string connection)
       (alist-get 'sql-database)
       car
       eval))

(defmacro with-open-read-only-buffer (&rest body)
  (declare (indent defun))
  `(progn
     (when buffer-read-only
       (setq buffer-read-only nil))
     (goto-char (point-max))
     ,@body
     (read-only-mode)))

(defun run-sql-query (connection query-string)
  "Run QUERY-STRING against CONNECTION."
  (let ((command (format "psql %s -P pager=off -c \"%s\""
			 connection query-string)))
    (shell-command-to-string command)))

(defun trim-whitespace (string)
  (->> string
       (replace-regexp-in-string "\s+$" "")
       (replace-regexp-in-string "^\s+" "")))

(defun parse-sql-query-output (output-string)
  (let ((rows (->> (split-string output-string "\n")
		   cddr
		   reverse
		   cdddr
		   reverse
		   (mapcar (lambda (s) (split-string s "\|")))))
	(result '()))
    (dolist (row rows)
      (when (> (length row) 1)
	(push (mapcar #'trim-whitespace row) result)))
    (reverse result)))

(defun all-but (pos list)
  (let ((result '())
	(current-pos 0))
    (dolist (item list)
      (unless (equal current-pos pos)
	(push item result))
      (setq current-pos (+ current-pos 1)))
    (reverse result)))

(defun group-by (pos lists)
  (let ((result-alist '()))
    (dolist (list lists)
      (let ((group-item  (nth pos list))
	    (rest        (all-but pos list)))
	(if-let ((other-items (alist-get
			       group-item
			       result-alist
			       nil
			       nil
			       'equal)))
	    (setf (alist-get group-item result-alist nil nil 'equal)
		  (cons rest other-items))
	  (push `(,group-item . (,rest)) result-alist))))
    result-alist))

(defun generate-table-names (sql-query-results)
  (let ((result '()))
    (dolist (schema sql-query-results)
      (let ((schema-string (car schema))
	    (tables        (cdr schema)))
	(dolist (table tables)
	  (push
	   (format "%s.%s"
		   (upcase schema-string)
		   (upcase (car table)))
	   result))))
    result))

(defun sql-session--force-complete (choices input)
  (let* ((end-idx     (length input))
	 (completions '())
	 (max-input   nil))
    (dolist (choice choices)
      (when (equal (substring choice 0 end-idx) input)
	(push choice completions)))
    (dolist (completion completions)
      (if max-input
	  (let ((max-idx   (length max-input))
		(current-idx 1)
		(done?     nil))
	    (while (and (not (< max-idx current-idx))
			(not done?))
	      (let ((compl-substring (substring
				      completion 0 current-idx))
		    (max-substring (substring
				    max-input 0 current-idx)))
		(unless (equal max-substring compl-substring)
		  (setq max-input (substring
				   max-substring 0 (- current-idx 1))
			done?     t)))
	      (setq current-idx (inc current-idx))))
	(setq max-input completion)))
    max-input))

(defun autocomplete-table ()
  ;; Assumes you are at the end of
  ;; the table name
  (interactive)
  (let ((words-include-escapes t)
	(do-something?         nil)
	(start                 nil)
	(end                   nil))
    (save-excursion
      (setq end (point))
      (if (equal (char-before) ?\s)
	  (backward-word)
	(progn
	  (re-search-backward (regexp-quote " "))
	  (setq start (+ 1 (point)))
	  (backward-word)))
      (when (member (downcase (word-at-point)) '("from" "join"))
	(setq do-something? t)))
    (when do-something?
      (let* ((already-typed (if start
				(upcase (buffer-substring start end))
			      ""))
	     (extended-typed (sql-session--force-complete
			      *current-tables* already-typed))
	     (table          (ido-completing-read
			      "Table: "
			      *current-tables*
			      nil
			      nil
			      extended-typed)))
	(if start
	    (progn
	      (goto-char start)

	      (kill-region start end)
	      (insert table))
	  (insert table))))))

(defun get-tables! (connection)
  "Get the list of tables for a CONNECTION."
  (let* ((connection-string (get-connection-information connection))
	 (query
	  "SELECT schemaname, tablename FROM pg_catalog.pg_tables;")
	 (results (->> query
		       (run-sql-query connection-string)
		       parse-sql-query-output
		       (group-by 0))))
    (setq *current-tables* (generate-table-names results))
    results))

(defun sql-session-get-tables ()
  (interactive)
  (unless *current-connection*
    (error "No SQL connection estabished"))
  (let ((buffer (current-buffer)))
    (switch-to-buffer *tables-buffer*)
    (kill-region (point-min) (point-max))
    (let* ((schemas             (get-tables! *current-connection*))
	   (sorted-schema-names (sort
				 (mapcar #'car schemas) #'string<)))
      (with-open-read-only-buffer
	(org-mode)
	(dolist (schema-string sorted-schema-names)
	  (let ((tables        (alist-get
				schema-string schemas nil nil #'equal)))
	    (insert (format "* %s\n" schema-string))
	    (dolist (table (sort (mapcar #'car tables) #'string<))
	      (insert (format "** %s\n" table)))))
	(evil-close-folds)))
    (switch-to-buffer buffer)))

(defun show-tables-buffer (orientation)
  (if-let ((sql-window (get-buffer-window *sql-buffer*)))
      (progn
	(select-window sql-window)
	(let ((current-window
	       (if (equal orientation :vertical)
		   (split-window-right 120)
		 (split-window-below))))
	  (select-window current-window)
	  (switch-to-buffer *tables-buffer*)))
    (error (format "\"%s\" buffer is not visible" *sql-buffer*))))

(defun hide-tables-buffer (window)
  (select-window window)
  (delete-window))

(defun sql-session-toggle-tables-buffer ()
  (interactive)
  (let ((current-window (selected-window)))
    (if-let ((tables-window (get-buffer-window *tables-buffer*)))
	(hide-tables-buffer tables-window)
      (show-tables-buffer *sql-session-orientation*))
    (select-window current-window)))

(defun sql-session--vertical-view (sql-code-buffer sql-connection-buffer)
  (delete-other-windows)
  (switch-to-buffer sql-code-buffer)
  (split-window-below)
  (other-window 1)
  (switch-to-buffer sql-connection-buffer)
  (other-window -1)
  (enlarge-window -10)
  (setq *sql-session-orientation* :vertical)
  (show-tables-buffer *sql-session-orientation*)
  (select-window (get-buffer-window sql-code-buffer)))

(defun sql-session--horizontal-view (sql-code-buffer sql-connection-buffer)
  (delete-other-windows)
  (switch-to-buffer sql-code-buffer)
  (split-window-right)
  (other-window 1)
  (switch-to-buffer sql-connection-buffer)
  (other-window 1)
  (setq *sql-session-orientation* :horizontal)
  (show-tables-buffer *sql-session-orientation*)
  (select-window (get-buffer-window sql-code-buffer)))

(defun sql-session-reset-window-layout ()
  (interactive)
  (sql-session--reset-window-layout-internal :vertical :horizontal))

(defun sql-session-toggle-orientation ()
  (interactive)
  (sql-session--reset-window-layout-internal :horizontal :vertical)
  (cond ((equal *sql-session-orientation* :horizontal)
	 (setq *sql-session-orientation* :vertical))
	((equal *sql-session-orientation* :vertical)
	 (equal *sql-session-orientation* :horizontal))))

(defun sql-session--reset-window-layout-internal
    (vertical-keyword horizontal-keyword)
  (let ((sql-connection (get-buffer *sqli-connection-buffer*))
	(sql            (get-buffer *sql-buffer*))
	(tables         (get-buffer *tables-buffer*)))
    (when (and sql-connection sql tables)
      (cond ((equal  *sql-session-orientation* vertical-keyword)
	     (sql-session--vertical-view sql sql-connection))
	    ((equal *sql-session-orientation* horizontal-keyword)
	     (sql-session--horizontal-view sql sql-connection))))))

(defun sql-session-start-session ()
  "Create a sql-session.

  NOTE: This function assumes
  that a sql-connection-alist
  has been defined"
  (interactive)
  (when (not sql-connection-alist)
    (let ((start "Package sql-session cannot be used")
	  (end   " without setting sql-connection-alist"))
      (error (format "%s%s" start end))))
  (setq *current-connection* (sql-read-connection "connection: "))
  (let* ((sqli-buffer (sql-connect *current-connection*))
	 (_ (switch-to-buffer *sql-buffer*)))
    (sql-mode)
    (setq *sqli-connection-buffer* sqli-buffer)
    ;; So sql knows theres a connection
    (setq sql-buffer sqli-buffer)
    (sql-session-get-tables)
    (run-hooks 'sql-set-sqli-hook)
    (sql-session-reset-window-layout)))

(defun sql-session-save-session ()
  (interactive)
  (save-window-excursion
    (when-let ((sql-buffer (get-buffer *sql-buffer*)))
      (let* ((file-prefix
	      "/Users/andrewparisi/Documents/notes/sql-sessions/")
	     (filename (format-time-string  "%m-%d-%Y"))
	     (filepath (concat file-prefix filename))
	     (to-write nil))
	(cl-do ((current-idx 1 (+ current-idx 1))
		(current-path
		 (concat filepath ".sql")
		 (concat
		  filepath "-" (number-to-string current-idx) ".sql")))
	    ((not (file-exists-p current-path))
	     (setq to-write current-path))
	  ())
	(switch-to-buffer sql-buffer)
	(write-file to-write)))))

(defun sql-session-quit-session ()
  (interactive)
  (switch-to-buffer *sqli-connection-buffer*)
  (insert "\\q")
  (comint-send-input)
  (kill-buffer *sqli-connection-buffer*)
  (setq *current-connection* nil)
  (setq *sqli-connection-buffer* nil)
  (kill-buffer *tables-buffer*)
  (setq *current-tables* nil)
  (let ((save-session? (y-or-n-p
			"Would you like to save this session? ")))
    (if save-session?
	(progn
	  (sql-session-save-session)
	  (kill-buffer))
      (kill-buffer *sql-buffer*)))
  (delete-other-windows))


(provide 'sql-session)
;;; sql-session.el ends here
