(tool-bar-mode -1)
(menu-bar-mode -1)

(when window-system
  (scroll-bar-mode -1))
(display-time-mode 1)
(setq inhibit-splash-screen t)

(global-display-line-numbers-mode)
(setq-default word-wrap t)

(turn-on-font-lock)
(show-paren-mode 1)
(setq-default indicate-empty-lines t)

(ignore-errors (set-frame-font "Menlo-14"))


(defvar *dispaly/keyword-function-map*
  '(:constant font-lock-constant-face
	      :function font-lock-function-name-face
	      :keyword  font-lock-keyword-face
	      :builtin  font-lock-builtin-face
	      :type     font-lock-type-face
	      :fringe   fringe))

(defun generate-face-attribute (type attribute-plist)
  (let ((result '())
	(foreground (plist-get attribute-plist :foreground))
	(background (plist-get attribute-plist :background))
	(weight     (plist-get attribute-plist :weight)))
    (when weight
      (push weight result)
      (push :weight result))
    (when background
      (push background result)
      (push :background result))
    (when foreground
      (push foreground result)
      (push :foreground result))
    (push 'nil result)
    (push (plist-get *dispaly/keyword-function-map* type) result)
    (cons 'set-face-attribute result)))

(defmacro colors! (&rest color-config)
  (let ((result '())
	(background   (plist-get color-config :background))
	(foreground   (plist-get color-config :foreground))
	(comment      (plist-get color-config :comment))
	(string       (plist-get color-config :string))
	(constant     (plist-get color-config :constant))
	(fn           (plist-get color-config :function))
	(keyword      (plist-get color-config :keyword))
	(type         (plist-get color-config :type))
	(builtin      (plist-get color-config :builtin))
	(fringe       (plist-get color-config :fringe))
	(transparency (plist-get color-config :transparency)))
    (when background
      (push `(set-background-color ,background) result))
    (when foreground
      (push `(set-foreground-color ,foreground) result))
    (when comment
      (push `(set-face-foreground font-lock-comment-face ,comment) result))
    (when string
      (push `(set-face-foreground font-lock-string-face ,string) result))
    (when constant
      (push (generate-face-attribute :constant constant) result))
    (when fn
      (push (generate-face-attribute :function fn) result))
    (when keyword
      (push (generate-face-attribute :keyword keyword) result))
    (when type
      (push (generate-face-attribute :type type) result))
    (when builtin
      (push (generate-face-attribute :builtin builtin) result))
    (when fringe
      (push (generate-face-attribute :fringe fringe) result))
    (when transparency
      (let ((letter (car transparency))
	    (num1   (cadr transparency))
	    (num2   (caddr transparency)))
;;	(push `(add-to-list default-frame-alist '(,letter ,num1 ,num2)) result)
	(push `(set-frame-parameter (selected-frame) ,letter '(,num1 ,num2)) result)
	))
    ;;(message (format "COLORS: %s" result))
    (cons 'progn result)))

(defmacro modeline! (&rest mode-line-config)
  ;; TODO: Ensure that users can specify the order
  ;; of items in the mode-line
  (let ((result '())
	(file-status-format (plist-get mode-line-config :file-status))
	(line-number?       (plist-get mode-line-config :line-number?))
	(major-mode-format  (plist-get mode-line-config :major-mode-format))
	(minor-modes?       (plist-get mode-line-config :minor-modes?))
	(time-format        (plist-get mode-line-config :time-format)))
    (when time-format
      (push time-format result))
    (when minor-modes?
      (push minor-mode-alist result))
    (when major-mode-format
      (push "%m" result))
      ;;(push major-mode-format result))
    (when line-number?
      (push "L%l" result))
    (when  file-status-format
      (push file-status-format result))
    (push 'mode-line-format result)
    (push 'setq-default result)))
