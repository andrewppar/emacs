(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))
(require 'package)
(package-initialize)

(add-to-list
 'package-archives
 '("org" . "http://orgmode.org/elpa/") t)

(add-to-list
 'package-archives
 '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/") t)

(setq read-process-output-max (* 1024 1024))

(defun install-core ()
  (setq package-list '(evil use-package))

  ;; activate all the packages (in particular autoloads)
  (package-initialize)

  ;; fetch the list of packages available
  (unless package-archive-contents
    (package-refresh-contents))

  ;; install the missing packages
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

(defvar *packages* '())

(defun read-packages ()
  (find-file "~/.emacs.d/packages")
  (let ((package-string (buffer-substring
			 (point-min) (point-max))))
    (kill-this-buffer)
    (split-string package-string "\n")))

(defun write-packages (packages)
  (find-file "~/.emacs.d/packages")
  (erase-buffer)
  (dolist (package packages)
    (insert (format "%s\n" package)))
  (save-buffer)
  (kill-this-buffer))

(defun sync-packages ()
  (let ((to-delete (read-packages))
	(current-packages *packages*))
    ;;(message (format "CUR: %s" current-packages))
    (dolist (package current-packages)
      (setq to-delete (remove package to-delete)))
    ;;(message (format "CURRENT: %s" current-packages))
    (dolist (package to-delete)
      (let ((package-spec (cadr
			   (assoc
			    (intern package)
			    package-alist))))
	(when package-spec
	  (package-delete package-spec))))
    (write-packages current-packages)))

(defun save-package (module-name)
  (let ((packages (read-packages)))
    (if (member module-name packages)
	(write-packages packages)
      (write-packages (cons module-name packages)))))

(defmacro module! (module-name &rest args)
  (declare (indent defun))
  (let ((start (gensym "start")))
    (setq *packages* (cons (symbol-name module-name) *packages*))
    `(progn
       (let ((,start (float-time)))
	 (message (format "Loading %s..." ',module-name)) 
	 (use-package ,module-name ,@args)
	 (message (format "Done loading %s ... %s" ',module-name (- (float-time) ,start)))))))
