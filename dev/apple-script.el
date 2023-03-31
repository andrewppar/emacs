(defmacro tell (command application context &rest body)
  (declare (indent 3))
  (let ((depth (gensym))
	(padding (gensym))
	(new-depth (gensym))
	(acc       (gensym))
	(form      (gensym)))
  `(let* ((,depth (or (plist-get ,context :depth) 0))
	  (,padding (make-string (* ,depth 2) ?\s))
	  (,new-depth (+ ,depth 1)))
      (setq ,context (plist-put ,context :depth ,new-depth))
      (format "%stell %s \"%s\"\n%s%send tell\n"
	      ,padding
	      ,command
	      ,application
	      (if-let ((result (let ((,acc ""))
				 (dolist (,form ,body)
				   (setq ,acc (concat ,acc ,form)))
				 acc)))
		  result
		"")
	      ,padding))))

(defmacro set (variable value context &rest body)
  (let ((depth (gensym))
	(padding (gensym)))
    `(let* ((,depth (or (plist-get ,context :depth) 0))
	   (,padding (make-string (* ,depth 2) ?\s)))
       (format "%sset %s to %s\n%s"
	       ,padding
	       ,variable
	       ,value
	       (if-let ((result (progn ,@body)))
		   result
		 "")))))

(defmacro write (text context &rest body)
  (let ((depth (gensym))
	(padding (gensym)))
    `(let* ((,depth (or (plist-get ,context :depth) 0))
	    (,padding (make-string (* ,depth 2) ?\s)))
       (format "%swrite text \"%s\"\n%s"
	       ,padding
	       ,text
	       (if-let ((result (progn ,@body)))
		   result
		 "")))))

(let ((ctx (plist-put nil :depth 0)))
		  (tell "application" "iTerm2" ctx
		    (set "newWindow" "(create window with default profile)" ctx)
		    (tell "current" "session of newWindow" ctx
		      (write "echo 'It works'" ctx))))






tell application "Terminal"
  activate
  do script "ssh user@server.com"
  -- // write user's password
  -- // write some linux commands to remote server
end tell
