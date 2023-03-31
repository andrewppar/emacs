;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     Timesheet     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interact With Timeseheet Database
;;
;; This module allows emacs to interact
;; with the clojure timesheet database.
;;
;; COMPLETED:
;; - Create function hit clojure api
;; - Find way of using org calendar to select date
;; - Create way of showing standard tasks for date
;; - Create way of showing standard tasks for today
;; - Create way of adding tasks to database
;; - Create way of summarizing tasks for date in range
;; - Create way of removing task from database
;; - Create awy of editing task in database
;;
;; TODOS:

(require 'org)
(require 'request)

;;;;;;;;;;;;;;;;;;;;;
;;;    Globals    ;;;
;;;;;;;;;;;;;;;;;;;;;

(defvar *timesheet-server* "localhost")
(defvar *timesheet-port* "3000")

(defvar *timesheet-projects*
  '("Personal"
    "Cisco"
    "Klezmer Archives"))

;;;;;;;;;;;;;;;;;;;;;
;;; Adding Tasks  ;;;
;;;;;;;;;;;;;;;;;;;;;

(defun log-task (start-time end-time)
  (interactive "sStart Time: \nsEnd Time: \n")
  (let* ((date (format-time-string "%m-%d-%Y"))
	 (project (ido-completing-read
                   "Select Project: " *timesheet-projects*))
         (jira (if (member project '())
		   (ido-completing-read
		    "JIRA: " (cons "" (jira-my-jira-issues)))
		 ""))
         (raw-description (read-string "Task Description: "))
         (description (if (equal jira "")
                          raw-description
                        (concat jira ": " raw-description))))
    (add-task date start-time end-time project description)))

(defun add-task (date start-time end-time project description)
  (run-timesheet-command "add" `(("date" . ,date)
                                 ("start" . ,start-time)
                                 ("end" . ,end-time)
                                 ("group" . ,(url-hexify-string  project))
                                 ("description" . ,(url-hexify-string description)))
                         t))


(defvar current-layout)
(defvar start-time)
(defvar end-time)
(defvar group)
(defvar description)
(defvar jira)

(defun new-log-task ()
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'current-layout)
  (setq current-layout (current-window-configuration))
  (make-local-variable 'start-time)
  (make-local-variable 'end-time)
  (make-local-variable 'group)
  (make-local-variable 'description)
  (make-local-variable 'jira)

  (delete-other-windows)
  (switch-to-buffer "*Log Task*")
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)

  (widget-insert "Log a Task.\n\n")
  (setq start-time
	(widget-create 'editable-field
			:size 13
			:format "Start Time: %v\n"))
  (setq end-time
	(widget-create 'editable-field
			:size 13
			:format "End Time: %v\n"))
  (widget-insert "\nJIRA: \n")
  (apply #'widget-create 'radio-button-choice
           :value ""
           :notify (lambda (widget &rest ignore)
                     (setq jira (widget-value widget)))
	   (let ((values `((editable-field :menu-tag "Other: " ""))))
	     (dolist (ticket (jira-my-jira-issues))
	       (push `(item ,ticket) values))
	     values))
  (widget-insert "\nProject:\n")
  (apply #'widget-create 'radio-button-choice
           :value ""
           :notify (lambda (widget &rest ignore)
                     (setq group (widget-value widget)))
	   (let ((values `()))
	     (dolist (project *timesheet-projects*)
	       (push `(item ,project) values))
	     values))
  (widget-insert "\n")
  (setq description
	(widget-create 'editable-field
				   :size 13
				   :format "Description: %v\n"))

  (widget-create 'push-button
                  :notify (lambda (&rest ignore)
			    (let* ((start (widget-value start-time))
				  (end   (widget-value end-time))
				  (date (format-time-string "%m-%d-%Y"))
				  (desc-string (widget-value description))
				  (desc (if jira
					    (format "%s: %s" jira desc-string)
					  (format "%s" desc-string))))
			      (add-task date start end group desc)
			      (kill-buffer)
			      (set-window-configuration current-layout)))
                  "Log")

  (use-local-map widget-keymap)
  (widget-setup))

;;;;;;;;;;;;;;;;;;;;;;
;;; Deleting Tasks ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defun delete-task-at-point ()
  (interactive)
  (cl-multiple-value-bind (date start-time end-time)
      (time-info-at-point)
    (delete-task date start-time end-time)))

(defun get-start-time-from-string (line)
  (let ((times (car (split-string line "*"))))
    (string-trim
     (car (split-string times " -- ")))))

(defun get-end-time-from-string (line)
  (let ((times (car (split-string line "*"))))
    (string-trim
     (cadr (split-string times " -- ")))))

(defun delete-task (date start-time  end-time)
  (let* ((keys `(("date" . ,date)
                 ("start" . ,start-time)
                 ("end" . ,end-time)))
         (results (run-timesheet-command "delete" keys t)))
    ;;; ((task-list . [((date (year . 2021) (month . 5) (day . 13)) (tasks . [((group . Intro) (tasks . [DPS-115: Write up progress report on ticket]))]) (time . 0.25))]))
    results))

(defun delete-task-at-point-gathering-data ()
  (cl-multiple-value-bind (date start-time end-time)
      (time-info-at-point)
    (let* ((deleted-task-json (delete-task
                               date start-time end-time))
           (task-list         (alist-get 'task-list deleted-task-json))
           (tasks-for-date    (aref task-list 0))
           (tasks             (alist-get 'tasks tasks-for-date))
           (group             (alist-get 'group (aref tasks 0)))
           (description       (aref (alist-get 'tasks (aref tasks 0)) 0)))
      (cl-values date start-time end-time group description))))

;;;;;;;;;;;;;;;;;
;;; Edit Task ;;;
;;;;;;;;;;;;;;;;;

(defun edit-task-description-at-point (new-description)
  (interactive "sNew Description: ")
  (cl-multiple-value-bind (date start-time end-time group old-description)
      (delete-task-at-point-gathering-data)
    (add-task date start-time end-time group new-description)))

(defun edit-task-start-time-at-point (new-time)
  (interactive "sNew Start Time: ")
  (cl-multiple-value-bind (date start-time end-time group description)
      (delete-task-at-point-gathering-data)
    (add-task date new-time end-time group description)))

(defun edit-task-end-time-at-point (new-time)
  (interactive "sNew End Time: ")
  (cl-multiple-value-bind (date start-time end-time group description)
      (delete-task-at-point-gathering-data)
    (add-task date start-time new-time group description)))

(defun edit-task-group-at-point ()
  (interactive)
  (let ((new-group (ido-completing-read
                    "Select Project: " *timesheet-projects*)))
    (cl-multiple-value-bind (date start-time end-time group description)
        (delete-task-at-point-gathering-data)
      (add-task date start-time end-time new-group description))))

(defun time-info-at-point ()
  (let* ((current-line (thing-at-point 'line t))
         (date (format-time-string "%m-%d-%Y"))
         (start-time   (get-start-time-from-string current-line))
         (end-time     (get-end-time-from-string current-line)))
    (cl-values date start-time end-time)))

;;;;;;;;;;;;;;;;;;;;;
;;; Viewing Tasks ;;;
;;;;;;;;;;;;;;;;;;;;;

(defun task-log-for-date ()
  (interactive)
  (let* ((date     (timesheet-get-date "Date: "))
         (log-name (format "*%s's Task Log" date)))
    (task-log-for-date-internal date log-name)))

(defun task-log ()
  (interactive)
  (let* ((date     (format-time-string "%m-%d-%Y"))
         (log-name "*Todays Task Log*"))
    (task-log-for-date-internal date log-name)))

(defun timesheet-get-time-difference
    (start-hour start-minute end-hour end-minute)
  (let* ((start-hour-int   (string-to-number (format "%s" start-hour)))
	 (start-minute-int (string-to-number (format "%s" start-minute)))
	 (end-hour-int     (string-to-number (format "%s" end-hour)))
	 (end-minute-int   (string-to-number (format "%s" end-minute)))
	 (start-time       (+ start-minute-int (* start-hour-int 60)))
	 (end-time         (+ end-minute-int (* end-hour-int 60))))
    (- end-time start-time)))

(defun timesheet-minutes-to-hours (minutes)
  (let ((hours   (floor (/ minutes 60)))
	(minutes (mod minutes 60)))
    (format "%s:%s" hours minutes)))

(defun task-log-for-date-internal (date log-name)
  (let* ((payload       (get-tasks-for-date-internal date))
	 (group-times '()))

    (with-open-task-log log-name
      (doarray group task-payload
        (let ((group-key  (alist-get 'group group))
              (tasks      (alist-get 'tasks group))
	      (group-time 0))
          (insert (format "{%s\n" group-key))
          (doarray task tasks
            (let ((start (alist-get 'start task))
                  (end   (alist-get 'end task))
                  (desc  (alist-get 'description task)))
              (cl-multiple-value-bind (start-hour start-minute)
                  (time-get-hour-and-minute start)
                (cl-multiple-value-bind (end-hour end-minute)
                    (time-get-hour-and-minute end)
                  (insert (format "%s:%s -- %s:%s * %s\n"
                                  start-hour start-minute end-hour end-minute desc))
		  (setq group-time
			(+ group-time
			   (timesheet-get-time-difference
			    start-hour start-minute end-hour end-minute)))))))
	  (push `(,group-key . ,group-time) group-times)
          (insert (format "}\n\n"))))
      (dolist (group-item group-times)
	(let ((group-key     (car group-item))
	      (group-minutes (cdr group-item)))
	  (insert (format "%s: %s\n"
			  group-key
			  (timesheet-minutes-to-hours group-minutes)))))
      (insert (format "\nTOTAL TIME: %s\n\n" total-time))
      (unless (= (length gaps) 0)
        (insert "GAPS: \n\n")
        (doarray gap gaps
          (let ((start (alist-get 'start gap))
                (end   (alist-get 'end gap)))
            (cl-multiple-value-bind (start-hour start-minute)
                (time-get-hour-and-minute start)
              (cl-multiple-value-bind (end-hour end-minute)
                  (time-get-hour-and-minute end)
                (insert (format "%s:%s -- %s:%s\n"
                                start-hour start-minute end-hour end-minute))))))))))

(defun time-get-hour-and-minute (json-time)
  (let* ((raw-hour  (alist-get 'hour json-time))
         (raw-minute (alist-get 'minute json-time))
         (hour       (if (< raw-hour 10) (format "0%s" raw-hour) raw-hour))
         (minute     (if (< raw-minute 10) (format "0%s" raw-minute) raw-minute)))
    (cl-values hour minute)))


(defun get-tasks-for-date ()
  (interactive)
  (let ((date (timesheet-get-date "Date:  ")))
    (get-tasks-for-date-internal date)))

(defun timesheet-get-date (prompt)
  "Get a date at sing `org-read-date' with its optional argument
of TO-TIME so that the user can customize the date format more easily."
  (let ((time (org-read-date nil 'to-time nil prompt)))
    (format-time-string "%m-%d-%Y" time)))


(defun get-tasks-for-date-internal (date)
  (run-timesheet-command
   "search" `(("start_date" . ,date) ("end_date" . ,date)) t))

(defun get-tasks-in-range ()
  "Get the tasks from a start date to an end date."
  (interactive)
  (let* ((start   (timesheet-get-date "Start Date: "))
         (end     (timesheet-get-date "End Date: "))
         (payload (get-tasks-in-range-internal start end)))
    (with-open-task-log (format "*Task Log:  %s --  %s" start end)
      (org-mode)
      (let ((dates     (alist-get 'task-list payload))
            (time      0)
            (max-group (--get-max-group payload)))
        (doarray date dates
          (let* ((date-json (alist-get 'date date))
                 (year      (alist-get 'year date-json))
                 (month     (alist-get 'month date-json))
                 (day       (alist-get 'day date-json))
                 (groups    (alist-get 'tasks date)))
            (doarray group groups
              (let* ((group-time (alist-get 'group-time group))
                     (group-name (alist-get 'group group))
                     (padding    (make-string (- max-group
                                                 (length group-name))
                                              ?\s)))
                (setq time (+ time group-time))
                (insert (format "* %s-%s-%s %s %s %s\n" month day year group-name padding group-time))
		(let ((tasks (alist-get 'tasks group)))
		  (doarray task tasks
		    (insert (format "** %s \n" task))))))))
        (insert "\n\n")
        (insert (format "* TOTAL TIME: %s" time))))))

(defun --get-max-group (payload)
  (let ((max-length 0))
    (doarray date (alist-get 'task-list payload)
      (doarray group (alist-get 'tasks date)
        (let ((group-length (length (alist-get 'group group))))
          (when (> group-length max-length)
            (setq max-length group-length)))))
    max-length))

(defun get-tasks-in-range-internal (start end)
  (run-timesheet-command "tasks-by-date" `(("start" . ,start)
                                           ("end" . ,end))
                         nil))

(defmacro with-open-task-log (log-name &rest body)
  (declare (indent defun))
  `(progn
     (switch-to-buffer ,log-name)
     (when buffer-read-only
       (setq buffer-read-only nil))
     (kill-region (point-min) (point-max))
     (goto-char 0)
     ,@body
     (read-only-mode)))


;;;;;;;;;;;;;;;;;
;;; Internals ;;;
;;;;;;;;;;;;;;;;;

(defun run-timesheet-command (postfix data post?)
  (let* ((url-base (format "%s:%s" *timesheet-server* *timesheet-port*))
	 (url      (if postfix (format "%s/%s" url-base postfix) url-base))
	 (type      (if post? "POST" "GET")))
    (-> url
	(request
	    :type type
	  :headers
	  '(("Content-Type" . "application/json"))
	  :data (json-encode data)
	  :sync t
	  :parser 'json-read)
	request-response-data)))

  ;;;;;;;;;;;;;;;;;;;
;;; Daily Slack ;;;
;;;;;;;;;;;;;;;;;;;

(defvar *timesheet-slack-workspace* nil)
(defvar *timesheet-slack-buffer* nil)

(defun timesheet-daily-slack-message ()
  (interactive)
  (let* ((today      (format-time-string "%m-%d-%Y"))
	 (notes-path "/Users/andrewparisi/Documents/notes/")
	 (filepath   (format "%s/daily-slack/%s.org" notes-path today))
	 (buffer     (find-file filepath)))
    (setq
     *timesheet-slack-workspace* (workspace-get-next-workspace-number)
     *timesheet-slack-buffer*    buffer)
    (workspace--add-workspace-no-prompt
     *timesheet-slack-workspace* "{} slack")
    (delete-other-windows)
    (split-window-right)
    (other-window 1)
    (org-agenda-list)
    (org-agenda-day-view)
    (other-window 1)
    (switch-to-buffer buffer)
    (flyspell-mode)
    (setup-org-file "DPS: Daily Slack Message")
    (goto-char (point-max))
    (insert "\n\n")
    (insert "Hey All! Here's what I'll be working on today:\n\n")))

(defun timesheet-daily-slack-message-quit ()
  (interactive)
  (if *timesheet-slack-workspace*
      (let ((home-ws-name (cdr (assoc 1 *workspaces*))))
	(save-window-excursion
	  (switch-to-buffer *timesheet-slack-buffer*)
	  (save-buffer))
	(workspace-switch-workspace home-ws-name)
	(if  (= *timesheet-slack-workspace* 1)
	    (kill-buffer *timesheet-slack-buffer*)
	  (let ((ws-name (format "%s: slack"
				 *timesheet-slack-workspace*)))
	    (workspace-remove-workspace ws-name)))
	(setq *timesheet-slack-workspace* nil
	      *timesheet-slack-buffer*    nil)
	(restore-organizer-layout))
    (message "No current slack session activated")))

;;;;;;;;;;;;;;;;;
;;; Utilities ;;;
;;;;;;;;;;;;;;;;;


(defmacro doarray (var array &rest body)
  (declare (indent defun))
  `(let ((idx 0)
         (last-idx (length ,array)))
     (while (< idx last-idx)
       (setq ,var (aref ,array idx))
       (setq idx (+ 1 idx))
       ,@body)))

(defmacro dpm (form)
  (let ((result (gensym "result")))
    `(let ((,result ,form))
       (message (format "%s" ,result))
       ,result)))




(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defvar widget-example-repeat)

(defun widget-example ()
  "Create the widgets from the Widget manual."
  (interactive)
  (switch-to-buffer "*Widget Example*")
  (kill-all-local-variables)
  (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "Here is some documentation.\n\n")
  (widget-create 'editable-field
                 :size 13
                 :format "Name: %v "	; Text after the field!
                 "My Name")
  (widget-create 'menu-choice
                 :tag "Choose"
                 :value "This"
                 :help-echo "Choose me, please!"
                 :notify (lambda (widget &rest ignore)
                           (message "%s is a good choice!"
                                    (widget-value widget)))
                 '(item :tag "This option" :value "This")
                 '(choice-item "That option")
                 '(editable-field :menu-tag "No option" "Thus option"))
  (widget-create 'editable-field
                 :format "Address: %v"
                 "Some Place\nIn some City\nSome country.")
  (widget-insert "\nSee also ")
  (widget-create 'link
                 :notify (lambda (&rest ignore)
                           (widget-value-set widget-example-repeat
                                             '("En" "To" "Tre"))
                           (widget-setup))
                 "other work")
  (widget-insert
   " for more information.\n\nNumbers: count to three below\n")
  (setq widget-example-repeat
        (widget-create 'editable-list
                       :entry-format "%i %d %v"
                       :notify
                       (lambda (widget &rest ignore)
                         (let ((old (widget-get widget
                                                ':example-length))
                               (new (length (widget-value widget))))
                           (unless (eq old new)
                             (widget-put widget ':example-length new)
                             (message "You can count to %d." new))))
                       :value '("One" "Eh, two?" "Five!")
                       '(editable-field :value "three")))
  (widget-insert "\n\nSelect multiple:\n\n")
  (widget-create 'checkbox t)
  (widget-insert " This\n")
  (widget-create 'checkbox nil)
  (widget-insert " That\n")
  (widget-create 'checkbox
                 :notify (lambda (&rest ignore) (message "Tickle"))
                 t)
  (widget-insert " Thus\n\nSelect one:\n\n")
  (widget-create 'radio-button-choice
                 :value "One"
                 :notify (lambda (widget &rest ignore)
                           (message "You selected %s"
                                    (widget-value widget)))
                 '(item "One") '(item "Another One.")
                 '(item "A Final One."))
  (widget-insert "\n")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (if (= (length
                                   (widget-value widget-example-repeat))
                                  3)
                               (message "Congratulation!")
                             (error "Three was the count!")))
                 "Apply Form")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (widget-example))
                 "Reset Form")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup))
