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
  (unless (file-exists-p bootstrap-file)
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
    (reverse result)))

(defun drop (n collection)
  "Drop N items from COLLECTION."
  (let ((result collection)
	(count 0))
    (while (and result (< count n))
      (setq result (cdr result)
	    count (+ count 1)))
    result))

(defun group-by (group-fn list) ; TODO allow users to specify test
  "Alist whose keys are result  GROUP-FN and values are items of LIST."
  (let ((result '()))
    (dolist (item list)
      (let* ((key (funcall group-fn item))
	     (value (alist-get key result nil t #'equal))
	     (new-value (cons item value)))
	(setf (alist-get key result nil nil #'equal) new-value)
	))
    result))

(defun every? (predicate collection)
  (let ((result t)
	(todo collection))
    (while (and result todo)
      (if-let ((item (car todo)))
	  (setq result (funcall predicate item)
		todo (cdr todo))
	(setq todo (cdr todo))))
    result))

(defun some? (test-fn list)
  "Check if TEST-FN hold of any element of LIST."
  (let ((result  nil)
	(todo    list)
	(done?   nil))
    (while (not done?)
      (if todo
	  (progn
	    (setq result (funcall test-fn (car todo)))
	    (if result
		(setq done? t)
	      (setq todo (cdr todo))))
	(setq done? t)))
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

(defun alist-get-in (alist keys)
  "Get the value of ALIST at nested KEYS."
  (let ((value (alist-get (car keys) alist nil nil #'equal)))
    (if (= (length keys) 1)
	value
      (alist-get-in value (cdr keys)))))

(defun alist-update (alist key update-function &rest args)
  "Change the value of ALIST at KEY with UPDATE-FUNCTION applied to ARGS."
  (let* ((old-value (alist-get key alist nil t #'equal))
	 (new-alist (assoc-delete-all key alist #'equal))
	 (new-value (apply update-function (cons old-value args))))
    (cons (cons key new-value) new-alist)))

(defun alist-update-in (alist keys update-function &rest args)
  "Change the vlaue of ALIST at nested KEYS with UPDATE-FUNCTION applied to ARGS."
  (let ((key (car keys)))
    (if (= (length keys) 1)
	(apply #'alist-update alist key  update-function args)
      (let* ((old-value (alist-get key alist nil t #'equal))
	     (new-alist (assoc-delete-all key alist #'equal))
	     (new-value (apply #'alist-update-in old-value (cdr keys) update-function args)))
	(cons (cons key new-value) new-alist)))))

(defun plist-keys (plist)
  "Get the keys for PLIST."
  (let ((key-position? t)
	(result      '()))
    (dolist (item plist)
      (when key-position?
	(push item result))
      (setq key-position? (not key-position?)))
    result))

(defmacro with-struct (class-name slots obj &rest body)
  "Bind slot names SLOTS in an instance OBJ of class CLASS-NAME, and execute BODY."
  (declare (indent 3))
  (let ((bindings '()))
    (dolist (slot slots)
      (push `(,slot (cl-struct-slot-value ',class-name ',slot ,obj)) bindings))
    `(let ,bindings (progn ,@body))))

(defmacro comment (&rest body)
  "Ignore BODY and return NIL."
  (declare '(indent defun))
  nil)

(defmacro defcached (name args &rest body)
  "Create a cached function with NAME, ARGS, and BODY."
  ;; THIS is broken because of the way hash tables work
  ;; Maybe use a class? OR something else that has fast lookup
  (declare (indent defun))
  (let ((result     (gensym))
	(cache-name (intern (format "%s-cache" name))))
    `(progn
       (defvar ,cache-name (make-hash-table))
       (defun ,name ,args
	 (if (gethash (list ,@args) ,cache-name)
	     (gethash (list ,@args) ,cache-name)
	   (let ((,result (progn ,@body)))
	     (puthash (list ,@args) ,result ,cache-name)
	     ,result))))))

(defmacro comp (&rest fns)
  "Create function that is the result of composing FNS."
  (let ((args-var (gensym)))
    (cl-labels ((make-comp-form (args functions)
		  (cond ((not functions) nil)
			((= (length functions) 1)
			 `(apply #',(car functions) ,args))
			(t
			 `(funcall #',(car functions) ,(make-comp-form args (cdr functions)))))))
      `(lambda (&rest ,args-var) ,(make-comp-form args-var fns)))))


(provide 'core)
;;; core.el ends here
