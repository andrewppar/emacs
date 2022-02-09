(require 'seq)

(defun display-recent-files ()
  (let ((recent-files (seq-take recentf-list 7))
	(index 1))
    (insert "Recent Files: \n\n")
    (dolist (path recent-files)
      (insert (format "%s - [[%s]] \n" index path))
      (setq index (+ index 1)))
    (insert "\n")))

(defun display-projects ()
    (insert "Projects: \n\n")
    (let ((index 1))
      (dolist (project (project--all-projects))
	(insert (format "%s - %s \n" index project))
	(setq index (+ index 1)))))

(defun memacs-splash (load-time)
  (switch-to-buffer "*MeMacs Splash*")
  (let ((start (point-min))
	(end   (point-max)))
    (org-mode)
    (kill-region start end)
    (insert "\n\n\n")
    (insert "88        88              88  88                         db                               88                                              \n")
    (insert "88        88              88  88                        d88b                              88                                              \n")
    (insert "88        88              88  88                       d8'`8b                             88                                              \n")
    (insert "88aaaaaaaa88   ,adPPYba,  88  88   ,adPPYba,          d8'  `8b      8b,dPPYba,    ,adPPYb,88  8b,dPPYba,   ,adPPYba,  8b      db      d8  \n")
    (insert "88\"\"\"\"\"\"\"\"88  a8P_____88  88  88  a8\"     \"8a        d8YaaaaY8b     88P'   `\"8a  a8\"    `Y88  88P'   \"Y8  a8P_____88  `8b    d88b    d8'  \n")
    (insert "88        88  8PP\"\"\"\"\"\"\"  88  88  8b       d8       d8\"\"\"\"\"\"\"\"8b    88       88  8b       88  88          8PP\"\"\"\"\"\"\"   `8b  d8'`8b  d8'   \n")
    (insert "88        88  \"8b,   ,aa  88  88  \"8a,   ,a8\"      d8'        `8b   88       88  \"8a,   ,d88  88          \"8b,   ,aa    `8bd8'  `8bd8'    \n")
    (insert "88        88   `\"Ybbd8\"'  88  88   `\"YbbdP\"'      d8'          `8b  88       88   `\"8bbdP\"Y8  88           `\"Ybbd8\"'      YP      YP      \n")
    (insert "\n\n\n")

    (insert (format "Load time: %s" load-time))
    (insert "\n\n\n")
    (display-projects)
    (insert "\n")
    (display-recent-files)
    (goto-char start)
    (read-only-mode)))
