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

;;;;;;;;;;;;;;;;;;;;;
;;; Installing Eirene

(defun install-core ()
  "Install Eirene."
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

;;; Manage knowledge of what packages are installed
(defvar *packages* '())

(defun read-packages ()
  "Read all packages from the `packages` db."
  (find-file "~/.emacs.d/packages")
  (let ((package-string (buffer-substring
			 (point-min) (point-max))))
    (kill-this-buffer)
    (split-string package-string "\n")))

(defun write-packages (packages)
  "Write installed PACKAGES to the `packages` db."
  (find-file "~/.emacs.d/packages")
  (erase-buffer)
  (dolist (package packages)
    (insert (format "%s\n" package)))
  (save-buffer)
  (kill-this-buffer))

(defun sync-packages ()
  "Ensure that `packages` db is in sync with current state."
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
  "Save a new package under MODULE-NAME to the `packages` db."
  (let ((packages (read-packages)))
    (if (member module-name packages)
	(write-packages packages)
      (write-packages (cons module-name packages)))))

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
