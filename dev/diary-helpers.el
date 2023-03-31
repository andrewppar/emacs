;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Organizer Customizations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This module contains custom ways
;; that I have developed for interacting
;; with my status.org file and agenda
;;

(defconst *status* "/Users/andrewparisi/org/status.org")


;; I think all of this is obsolete
;; TODO: Clean this up!
(defun org-goto-heading (heading &rest context)
  "Go to the org mode heading called HEADING
   where the parent heading paths to it are CONTEXT.
   e.g. (Today Schedule Once) goes to *** Today
   under * Schedule under ** Once"
  (interactive)
  (let ((org-tree        (org-collect-headers))
	(current-context (car context)))
    (org-goto-heading-internal heading org-tree current-context)))

(defun org-goto-heading-internal (heading tree context)
  (let ((result nil))
    (if (not context)
	(dolist (subtree tree)
	  (let ((current-header (string-trim (caar subtree))))
	    (when (equal current-header heading)
	      (setq result (cadar subtree)))))
      (let ((current-context (car context)))
	(dolist (subtree tree)
	  (let ((header (string-trim (caar subtree))))
	    (when (equal header current-context)
	      (setq result
		    (org-goto-heading-internal
		     heading (cdr subtree) (cdr context))))))))
    result))

(defun org-collect-headers ()
  (save-window-excursion
    (find-file *status*)
    (let ((result '()))
      (save-excursion
	(goto-char 0)
	(let ((done? nil))
          (while (not done?)
            (let ((next-header (org-get-next-header)))
	      (if (not next-header)
                  (setq done? t)
		(let ((header (car next-header))
		      (depth  (cadr next-header))
		      (pos    (caddr next-header)))
                  (setq result
			(insert-org-item header depth pos result))))))))
      (cdr (car result)))))

(defun insert-org-item (header depth position result)
  (let* ((current-item (get-nested-header-item depth result))
         (new-item      (cons `((,(substring-no-properties header) ,position) . ()) current-item)))
    (replace-header-item new-item depth result)))

(defun get-nested-header-item (depth result)
  (if (= depth 1)
      (cdar result)
    (get-nested-header-item (- depth 1) (cdar result))))

(defun replace-header-item (new-item depth result)
  (if (= depth 1)
      (let* ((old-item (car result))
             (new-item `(,(car old-item) . ,new-item)))
        (cons new-item (cdr result)))
    (cons `(,(car (car result)) . ,(replace-header-item new-item (- depth 1) (cdr (car result)))) (cdr result))))

(defun org-get-next-header ()
  (let ((next-header? (re-search-forward "^\*" nil t)))
    (when next-header?
      (let* ((line       (thing-at-point 'line))
             (line-items (split-string line " " t))
             (stars      (car line-items))
             (header     (mapconcat 'identity (cdr line-items) " "))
             (depth      (length stars)))
        (cl-values header depth (point))))))

(load "~/emacs-files/todoist.el")

(defun import-todoist (todoist-loc next-loc)
  (save-window-excursion
    (find-file *status*)
    (save-excursion
      (let ((start-pos (header-position todoist-loc))
	    (end-pos   (header-position next-loc))
	    (indent    (inc (length todoist-loc))))
	(goto-char start-pos)
	(forward-line 1)
	(setq start-pos (point))
	(kill-region start-pos end-pos)
	(-> (todoist-get-tasks)
	    (todoist-serialize-tasks indent)
	    insert)))))

(defun export-todoist (todoist-loc next-loc)
  (save-window-excursion
    (find-file *status*)
    (save-excursion
      (let ((start-pos (header-position todoist-loc))
	    (end-pos   (header-position next-loc)))
	(goto-char start-pos)
	(forward-line 1)
	(setq start-pos (point))
	(do-org-headers header
	  (let ((current-pos (point)))
	    (when (and (< start-pos current-pos)
		       (< current-pos end-pos))
	      (when (equal (org-entry-get current-pos "TODO") "DONE")
		(-> current-pos
		    (org-entry-get "ID")
		    todoist-close-task)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Calendar Entries ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun archive-calendar-items ()
  (interactive)
  (save-window-excursion
    (find-file *status*)
    (goto-char (org-goto-heading "Today" '("Schedule" "Once")))
    (forward-line)
    (beginning-of-line)
    (let ((start (point)))
      (goto-char (org-goto-heading "Notes"))
      (forward-line -1)
      (end-of-line)
      (let* ((end (point))
	     (archive-tasks (buffer-substring start end)))
	(kill-region start end)
	(save-buffer)))))

(defun add-calendar-item-for-day (title start-time end-time date)
  (let ((month (car date))
	(day   (cadr date))
	(year  (caddr date)))
    (save-window-excursion
      (find-file *status*)
      (goto-char (org-goto-heading "Today" '("Schedule" "Once")))
      (forward-line)
      (insert "\n")
      (forward-line -1)
      (if (or (equal start-time "")
	      (equal end-time ""))
	  (insert
	   (format "**** %s \n" title))
	(insert
	 (format "**** %s %s-%s\n" title start-time end-time)))
      (insert
       (format "<\%\%\%\%(equal date '(%s %s %s))>" month day year)))))

(defun add-calendar-item-for-today (title start-time end-time)
  (interactive "sTitle: \nsStart Time: \nsEnd Time: ")
  (let ((month (format-time-string "%m"))
	(day   (format-time-string "%d"))
	(year  (format-time-string "%Y")))
    (add-calendar-item-for-day
     title start-time end-time `(,month ,day ,year))))

(defun calendar-day-today ()
  (let ((month (string-to-number (format-time-string "%m")))
	(day   (string-to-number (format-time-string "%d")))
	(year  (string-to-number (format-time-string "%Y"))))
    `(,month ,day ,year)))

(defun calendar-next-day (calendar-day)
  (let* ((day (calendar-extract-day calendar-day))
         (month (calendar-extract-month calendar-day))
         (year  (calendar-extract-year calendar-day))
         (last-day (calendar-last-day-of-month month year)))
    (if (= day last-day)
        (if (= month 12)
            `(01 01 ,(inc year))
          `(,(inc month) 01 ,year))
      `(,month ,(inc day) ,year))))

(defun calendar-next-week (calendar-day)
  (let ((result calendar-day))
    (dotimes (i 7)
      (setq result (calendar-next-day result)))
    result))

(defun calendar-day-less-than? (day1 day2)
  (let ((first-day (calendar-extract-day day1))
        (second-day (calendar-extract-day day2))
        (month1 (calendar-extract-month day1))
        (month2 (calendar-extract-month day2))
        (year1  (calendar-extract-year  day1))
        (year2  (calendar-extract-year day2)))
    (if (= year1 year2)
        (if (= month1 month2)
            (< first-day second-day)
          (< month1 month2))
      (< year1 year2))))

(defmacro inc (number)
  `(+ ,number 1))

(defun calendar-nth-week? (day start-day n)
  (let ((week-number 0)
	(current-day start-day))
    (while (calendar-day-less-than? current-day day)
      (let ((current-week-number (if (= week-number (- n 1))
				     0
				   (inc week-number))))
	(setq week-number current-week-number
	      current-day (calendar-next-week current-day))))
    (= week-number 0)))

(defun calendar-other-week? (day start-day)
  (let ((other-week? t)
        (current-day start-day))
    (while (calendar-day-less-than? current-day day)
      (setq other-week? (not other-week?)
            current-day (calendar-next-week current-day)))
    other-week?))

;;;;;;;;;;;;;
;;; iCalBuddy

(defun get-todays-events ()
  (shell-command-to-string "icalbuddy -tf %H:%M  eventsToday"))

(defun get-next-weeks-events ()
  (shell-command-to-string
   "icalbuddy -tf %H:%M  -df %m/%d/%Y eventsToday+14"))

(defun reverse-range (n)
  (let ((index 0)
	(result ()))
    (while (< index n)
      (push index result)
      (setq index (+ index 1)))
    result))

(defun parse-title-line (line)
  (let* ((title-start (substring line 2))
	 (email-end   (length title-start))
	 (title-end   nil)
	 (paren-depth nil))
    (dolist (index (reverse-range email-end))
      (let ((char (aref title-start index)))
	(cond ((equal char ?\))
	       (if paren-depth
		   (setq paren-depth
			 (+ paren-depth 1))
		 (setq paren-depth 0)))
	      ((equal char ?\()
	       (if (equal paren-depth 0)
		   (setq title-end index)
		 (setq paren-depth (- paren-depth 1))))
	      (t
	       nil))))
    (string-trim-right
     (substring title-start 0 title-end))))

(defun time-line-with-date-internal? (line)
  (let* ((regex-one
	 "^.*at [0-9][0-9]:[0-9][0-9][[:space:]]*-[[:space:]]*[0-9][0-9]:[0-9][0-9][[:space:]]*$")
	 (match-one (string-match-p regex-one line))
	 (regex-two "^[[:space:]]*[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9].*$")
	 (match-two (string-match-p regex-two line)))
    (if (or match-one match-two)
	(or (when match-one (= match-one 0))
	    (when match-two (= match-two 0)))
      (and match-one match-two))))

(defun date-line? (raw-line)
  (let ((line (string-trim raw-line)))
    (or (equal line "today")
	(equal line "tomorrow")
	(equal line "day after tomorrow"))))

(defun time-line-with-date? (line)
  (or (time-line-with-date-internal? line)
      (date-line? line)))

(defun dateless-time-line? (line)
  (let* ((regex "^[[:space:]]*[0-9][0-9]:[0-9][0-9][[:space:]]*-[[:space:]]*[0-9][0-9]:[0-9][0-9][[:space:]]*$")
	 (match (string-match-p regex line)))
    (if match
	(= match 0)
      match)))

(defun time-line? (line)
  (or (dateless-time-line? line)
      (time-line-with-date? line)))

(defun parse-dateless-time-line (line)
  (let* ((regex                "[0-9][0-9]:[0-9][0-9]")
	 (start-time-start-idx (string-match-p regex line))
	 (start-time-end-idx   (+ start-time-start-idx 5))
	 (start-time           (substring
				line start-time-start-idx start-time-end-idx))
	 (rest-of-line         (substring line start-time-end-idx))
	 (end-time-start-idx   (string-match-p regex rest-of-line))
	 (end-time-end-idx     (+ end-time-start-idx 5))
	 (end-time             (substring
				rest-of-line end-time-start-idx end-time-end-idx)))
    `((start . ,start-time) (end . ,end-time))))

(defun parse-dated-time-line (line)
  (let* ((date-time (split-string line "at" "[\t \n]+"))
	 (raw-date  (string-trim (car date-time)))
	 (raw-time  (cadr date-time))
	 (start-end (when (and raw-time (dateless-time-line? raw-time))
		      (parse-dateless-time-line raw-time)))
	 (today     (calendar-day-today))
	 (date      (cond ((equal raw-date "today") today)
			  ((equal raw-date "tomorrow")
			   (calendar-next-day today))
			  ((equal raw-date "day after tomorrow")
			   (calendar-next-day
			    (calendar-next-day today)))
			  (t
			   (mapcar #'string-to-number
				   (split-string raw-date "/" "[ ]+"))))))
    (cons `(date . ,date) start-end)))

(defun parse-events (raw-output date?)
  (let* ((output-lines  (split-string raw-output "\n"))
	 (result        ())
	 (current-event ()))
    (dolist (line output-lines)
      (cond ((and (not (equal line ""))
		  (equal (substring line 0 1) "•"))
	     (progn
	       (when current-event
		 (push current-event result))
	       (setq current-event
		     `((title . ,(parse-title-line line))))))
	    ((if date? (time-line-with-date? line) (dateless-time-line? line))
	     (push `(time . ,(if date?
				 (parse-dated-time-line line)
			       (parse-dateless-time-line line)))
		   current-event))))
    (push current-event result)
    result))

(defun parse-todays-events ()
  (parse-events (get-todays-events) nil))

(defun parse-next-weeks-events ()
  (parse-events (get-next-weeks-events) t))

(defun refresh-calendar-items (timespan)
  (archive-calendar-items)
  (let ((new-events (cl-case timespan
		      (:today (parse-todays-events))
		      (:week  (parse-next-weeks-events)))))
    (unless (equal new-events "")
      (dolist (event new-events)
	(let* ((title     (alist-get 'title event))
	       (time      (alist-get 'time event))
	       (start     (alist-get 'start time))
	       (end       (alist-get 'end time))
	       (raw-date  (alist-get 'date time))
	       (date      (if raw-date raw-date (calendar-day-today))))
	  (if (or (not start) (not end))
	      (add-calendar-item-for-day title "" "" date)
	    (add-calendar-item-for-day title start end date)))))))

(defun refresh-calendar-items-today ()
  (interactive)
  (refresh-calendar-items :today))

(defun refresh-calendar-items-next-week ()
  (interactive)
  (refresh-calendar-items :week))



;;;;;;;;;;;;
;;; Holidays

(defun nth-day-from-easter (year distance)
  (let* ((century (1+ (/ year 100)))    ; the century of the year
         (shifted-epact			; age of moon for April 5...
          (% (+ 14
		(* 11 (% year 19))	; ...by Nicaean rule
                (-	 ; ...corrected for the Gregorian century rule
                 (/ (* 3 century) 4))
		(/	   ; ...corrected for Metonic cycle inaccuracy
                 (+ 5 (* 8 century)) 25)
                (* 30 century))		; keeps value positive
             30))
         (adjusted-epact		; adjust for 29.5 day month
          (if (or (zerop shifted-epact)
                  (and (= shifted-epact 1) (< 10 (% year 19))))
              (1+ shifted-epact)
            shifted-epact))
         (paschal-moon	; day after the full moon on or after March 21
          (- (calendar-absolute-from-gregorian (list 4 19 year))
             adjusted-epact))
         (abs-easter (calendar-dayname-on-or-before 0 (+ paschal-moon 7)))
         (greg (calendar-gregorian-from-absolute (+ abs-easter distance))))
    greg))

(defun nth-day-from-current-easter (distance)
  (let ((year (string-to-number (format-time-string "%Y"))))
    (nth-day-from-easter year distance)))

;;;;;;;;;;;;
;;; pomodoro

(defvar *pomodoro-buffer* "*Pomodoro*")
(defvar *current-timer* nil)

(defmacro with-open-pomodoro (&rest body)
  (declare (indent defun))
  `(progn
     (when buffer-read-only
       (setq buffer-read-only nil))
     (goto-char (point-max))
     (insert "\n")
     ,@body
     (read-only-mode)))

(defun new-pomodoro ()
  (switch-to-buffer *pomodoro-buffer*)
  (with-open-pomodoro
    (insert (format "Pomodoro: %s\n" (format-time-string "%m-%d-%Y")))
    (insert "-----------------\n")))


(defun pomodoro ()
  (interactive)
  (if (get-buffer *pomodoro-buffer*)
      (switch-to-buffer *pomodoro-buffer*)
    (new-pomodoro))
  (unless buffer-read-only
    (read-only-mode)))

(defmacro start-timer (prompt time)
  (let ((command (format "say %s" prompt)))
    `(setq *current-timer
	   (run-with-timer
	    time nil
	    (lambda ()
	      (shell-command ,command))))))

(defun pomodoro-send-message (start-message)
  (unless (get-buffer *pomodoro-buffer*)
    (save-window-excursion
      (new-pomodoro)))
  (save-window-excursion
    (switch-to-buffer *pomodoro-buffer*)
    (with-open-pomodoro
      (insert start-message))))

(defun pomodoro-start-work (&optional time)
  (interactive)
  (unless time
    ;; default time
    ;; is 25 minutes
    (setq time 1500))
  (pomodoro-send-message
   (format "Started work pomodoro at %s" (format-time-string "%H:%M")))
  (start-timer "time to rest" time))

(defun pomodoro-start-rest (&optional time)
  (interactive)
  (unless time
    ;; default time
    ;; is 5 minutes
    (setq time 300))
  (pomodoro-send-message
   (format "Started rest pomodoro at %s" (format-time-string "%H:%M")))
  (start-timer "time to focus" time))

(defun pomodoro-stop-timer ()
  (interactive)
  (if *current-timer*
      (cancel-timer *current-timer*)
    (message "No timer to cancel")))
