(load "~/.emacs.d/core.el")

;; For use with evil-collection
;; if evil-collections is removed from
;; default load packageds this can be removed
(setq evil-want-keybinding nil)

(require 'evil)
(evil-mode t)

(load "~/.emacs.d/modules.el")

;replace <esc> with jk.
(define-key evil-insert-state-map "j" #'cofi/maybe-exit)
(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "j")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?k)
			   nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?k))
	(delete-char -1)
	(set-buffer-modified-p modified)
	(push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
					      (list evt))))))))

(setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))

;;;;;;;;;;;;;;
;; Keybindings

(defun generate-which-key-binding-sexp (map trigger binding item type)
  (message (format "KeyBinding: %s %s %s %s %s" map trigger binding item type))
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
    (message (format "BINDINGS: %s" (cons 'progn (append labels bindings major-mode-bindings))))
    (cons 'progn (append labels bindings major-mode-bindings))))



;;;;;;;;;;;;;;;;;;;;;;;
;;; Default Keybindings

(which-key-map evil-normal-state-map ","
 :labels
 ("" "main menu"
  "w" "window"
  "b" "buffer"
  "m" "major mode")
  :default-bindings
  ("f"  'find-file
   "e"  'eval-buffer
   "0"  'delete-window
   "1"  'delete-other-windows
   "2"  'split-window-below
   "3"  'split-window-right
   "bd" 'kill-buffer
   "bx" 'kill-buffer-and-window
   "ww" 'other-window
   "wd" 'delete-window
   ))


