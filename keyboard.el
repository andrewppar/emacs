(load! "~/.emacs.d/core.el")

(require 'use-package)

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t))

(use-package which-key
  :ensure t
  :requires evil
  :config
  (set-face-attribute 'which-key-command-description-face nil :inherit nil)
  (setq which-key-idle-delay 0.1
		 which-key-separator " → ")
	   (which-key-mode))

;; replace <esc> with jk.
(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "j")
    (let ((evt
	   (read-event
	    (format "Insert %c to exit insert state" ?k)
	    nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?k))
	(delete-char -1)
	(set-buffer-modified-p modified)
	(push 'escape unread-command-events))
       (t
	(setq unread-command-events
	      (append unread-command-events
		      (list evt))))))))
(define-key
  evil-insert-state-map "j" #'cofi/maybe-exit)

;;;;;;;;;;;;;;
;; Keybindings

(defun generate-which-key-binding-sexp (map trigger binding item type)
  ;;(message (format "KeyBinding: %s %s %s %s %s" map trigger binding item type))
  (let ((binding-string (if (equal type :major-mode)
			    (concat trigger "m" binding)
			  (concat trigger binding)))
	(result         nil))
    (cond ((equal type :labels)
	   (setq result
		 `(which-key-add-keymap-based-replacements ,map ,binding-string ,item)))
    	  ((equal type :default-bindings)
    	   (setq result
		 `(define-key ,map ,binding-string ,item)))
	  ((equal type :major-mode)
	   (setq result
		 `(progn
		    (evil-define-key 'normal ,map ,(concat "m" binding) ,item)
		    (evil-define-key 'normal ,map ,binding-string ,item)))))
    result))

(defun get-which-key-item-internal (map trigger configuration-plist type)
  (let ((items (plist-get configuration-plist type))
	(result '()))
    (when items
      (cl-do ((binding (car items) (car rest))
	      (item (cadr items) (cadr rest))
	      (rest (cddr items) (cddr rest)))
	  ((not rest)
	   (push
	    (generate-which-key-binding-sexp map trigger binding item type)
	    result))
	(push
	 (generate-which-key-binding-sexp map trigger binding item type)
	 result))
      result)))

(defun make-labels (map trigger configuration-plist)
  (get-which-key-item-internal map trigger configuration-plist :labels))
      
(defun make-default-bindings (map trigger configuration-plist)
  (get-which-key-item-internal map trigger configuration-plist :default-bindings))

(defun make-major-mode-bindings (trigger configuration)
  (let ((intermediate-config (plist-get configuration :major-mode))
	(result             '()))
    (dolist (major-mode-config intermediate-config)
      (let ((major-map (car major-mode-config))
	    (configuration-plist `(:major-mode ,(cdr major-mode-config))))
	(setq result
	      (append (get-which-key-item-internal
		       major-map trigger configuration-plist :major-mode)
		      result))))
    result))
					
(defmacro which-key-map (map trigger &rest configuration)
  (let ((labels              (make-labels map trigger configuration))
	(bindings            (make-default-bindings map trigger configuration))
	(major-mode-bindings (make-major-mode-bindings trigger configuration)))
    ;;(message (format "BINDINGS: %s" (cons 'progn (append labels bindings major-mode-bindings))))
    (cons 'progn (append labels bindings major-mode-bindings))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Default Keybindings

(which-key-map evil-normal-state-map ","
 :labels
 ("" "main menu"
  "b" "buffer"
  "e" "eval"
  "r" "kill-ring"
  "m" "major mode")
  :default-bindings
  ("f"  'counsel-find-file
   "eb" 'eval-buffer
   "ed" 'eval-defun
   "0"  'delete-window
   "1"  'delete-other-windows
   "2"  'split-window-below
   "3"  'split-window-right
   "bd" 'kill-buffer
   "bx" 'kill-buffer-and-window
   "ry" 'counsel-yank-pop
   "a"  'other-window))


                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun generate-mm-bindings (mode bindings-plist)
  (let ((bindings (plist-get bindings-plist :bindings))
	(mode-map (intern-soft (concat
				(symbol-name mode)
				"-map")))
	(result '()))
    (when (and mode-map bindings)
      (do-bindings (key function bindings)
	(setq result
	      (append (list
		       (concat "m" key) function
		       (concat ",m" key) function)
		      result)))
      `(evil-define-key 'normal ,mode-map . ,result))))

(defun generate-mm-labels (mode bindings-plist)
  (let ((bindings (plist-get bindings-plist :labels))
	(result  '()))
    (when bindings
      (do-bindings (keys label bindings)
	(setq result
	      (append (list
		       (concat "m" keys) label
		       (concat ",m" keys) label)
		      result)))
      `(which-key-add-major-mode-key-based-replacements
	 ',mode . ,result))))

(defmacro major-mode-map (mode &rest bindings-plist)
  (declare (indent defun))
  (let ((bindings
	 (generate-mm-bindings mode bindings-plist))
	(labels
	 (generate-mm-labels   mode bindings-plist))
	(result '()))
    (setq result `(progn ,bindings ,labels))
    result))


(defmacro do-bindings (binding-list &rest body)
  (declare (indent defun))
  (let ((key (car binding-list))
	(val (cadr binding-list))
	(bindings (caddr binding-list)))
    `(cl-do ((,key (car ,bindings) (car next))
	     (,val (cadr ,bindings) (cadr next))
	     (next (cddr ,bindings) (cddr next)))
	 ((not next)
	  (progn
	    ,@body
	  ))
       (progn
	 ,@body))))
