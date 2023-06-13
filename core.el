;;; core.el --- Eirene's Core Functionality -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 20 July 2021
;; Homepage: N/A
;; Keywords: emacs
;; Package-Requires: ((emacs "28"))
;; SPDX-License-Identifier: MIT
;; Version: 3.0

;;; Commentary:
;;
;; Controls the core functionality of eirene
;;
;; In particular, it sets up a framework for package management,
;; installation, and syncing offline.
;;
;; It also contains utilities that I think are indispensible for
;; writing clear Emacs Lisp.
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;
;; Garbage Collection

;; Manage Garbage Collection to speed up initialization
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;;;;;;;;;;;
;; Packages

(defvar bootstrap-version)
(let* ((relative-file "straight/repos/straight.el/bootstrap.el")
       (bootstrap-file (expand-file-name relative-file  user-emacs-directory))
       (bootstrap-version 6))
  (unless (file-exists-p bootstrap-version)
    (with-current-buffer
	(url-retrieve-synchronously
 	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)

(setq read-process-output-max (* 1024 1024))

;;;;;;;;;;;;;;;;;;;;;
;;; Installing Eirene

(defun install-core ()
  "Install Eirene."
  (setq package-list '(evil use-package))
  (setq enable-package-at-startup nil)

  ;; install the missing packages
  (dolist (package package-list)
    (straight-use-package package)))

(defmacro use-package-wrapper! (module-name &rest args)
  "A Macro for wrapping `use-package`.

It takes MODULE-NAME and whatever ARGS would normally be sent to
`use-package`."
  (declare (indent defun))
  (let ((start  (gensym "start")))
    (setq *packages* (cons (symbol-name module-name) *packages*))
    `(progn
       (let ((,start (float-time)))
	 (message (format "Loading %s..." ',module-name))
	 (use-package ,module-name ,@args)
	 (message
	  (format "Done loading %s ... %s"
		  ',module-name
		  (- (float-time) ,start)))))))

(defun maybe-install-package (module-name module-dir)
  "Install MODULE-NAME to MODULE-DIR if it isn't already installed."
  (unless (package-installed-p module-name)
    (package-install-file module-dir)))

(defmacro simple-wrapper! (module-name &rest args)
  "A wrapper for just executing code."
  (declare (indent defun))
  (let ((start    (gensym "start"))
	(load-dir (plist-get args :load)))
    (when load-dir
      (maybe-install-package module-name load-dir))
    ;; TODO: Check for package and install if necessary - maybe we have a build flag?
    `(progn
       (let ((,start (float-time)))
	 (message (format "Loading %s..." ',module-name))
	 ,@args
	 (message (format "Done loading %s ... %s"
			  ',module-name
			  (- (float-time) ,start)))))))

(defmacro module! (module-name &rest args)
  "Declare that MODULE-NAME should be installed as a package.

ARGS are the settings for that module."
  (declare (indent 1))
  (pcase (car args)
    (:use-package
     (if (equal (car (cdr args)) 'nil)
	 `(simple-wrapper! ,module-name ,@(cddr args))
       `(use-package-wrapper! ,module-name ,@args)))
    (_
     `(use-package-wrapper! ,module-name ,@args))))

;;; Utilities

(defmacro -> (item &rest forms)
  "Thread ITEM through FORMS in the first argument place."
  (cond ((not forms)
	 item)
	((length= forms 1) (let ((form (car forms)))
	   (if (listp form)
	       `(,(car form) ,item ,@(cdr form))
	     (list form item))))
	(t
	 `(->
	   (-> ,item ,(car forms))
	   ,@(cdr forms)))))

(defmacro ->> (item &rest forms)
  "Thread ITEM through FORMS in the last argument place."
  (cond ((not forms)
	 item)
	((length= forms 1)
	 (let ((form (car forms)))
	   (if (listp form)
	       (reverse
		(cons item (reverse form)))
	     (list form item))))
	(t
	 `(->>
	   (->> ,item ,(car forms))
	   ,@(cdr forms)))))

(defun filter (test-fn list)
  "Filter items in LIST by TEST-FN."
  (let ((result '()))
    (dolist (item list)
      (when (funcall test-fn item)
	(push item result)))
    result))

(defmacro doarray (spec &rest body)
  "Iterate over SPEC '(var array)' executing BODY."
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (unless (consp spec)
    (signal 'wrong-type-argument (list 'consp spec)))
  (unless (<= 2 (length spec) 3)
    (signal 'wrong-number-of-arguments (list '(2 . 3) (length spec))))
  `(let ((idx 0)
         (last-idx (length ,(cadr spec))))
     (while (< idx last-idx)
       (setq ,(car spec) (aref ,(cadr spec) idx))
       (setq idx (+ 1 idx))
       ,@body)))

(defmacro doalist (spec &rest body)
  "Iterate over SPEC '(var alist)' executing BODY."
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (unless (consp spec)
    (signal 'wrong-type-argument (list 'consp spec)))
  (unless (<= 3 (length spec) 4)
    (signal 'wrong-number-of-arguments (list '(3 . 4) (length spec))))
  (let ((keys (gensym))
        (key  (gensym)))
    `(let ((,keys (mapcar #'car ,(caddr spec))))
       (dolist (,key ,keys)
         (let ((,(car spec) ,key)
               (,(cadr spec) (alist-get
                              ,key ,(caddr spec) nil nil #'equal)))
           ,@body)))))

(defmacro comment (&rest body)
  "Ignore BODY and return NIL."
  (declare '(indent defun))
  nil)

(provide 'core)
;;; core.el ends here
