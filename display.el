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

;; Color Theme

(defvar *display/keyword-function-map*
  '(:constant font-lock-constant-face
	      :function            font-lock-function-name-face
	      :keyword             font-lock-keyword-face
	      :builtin             font-lock-builtin-face
	      :type                font-lock-type-face
	      :mode-line          'mode-line
	      :mode-line-inactive 'mode-line-inactive
	      :font               'default
	      :fringe             'fringe))

(defun generate-face-attribute (type attribute-plist)
  (let ((result '())
	(foreground (plist-get attribute-plist :foreground))
	(background (plist-get attribute-plist :background))
	(height     (plist-get attribute-plist :height))
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
    (when height
      (push height result)
      (push :height result))
    (push 'nil result)
    (push (plist-get *display/keyword-function-map* type) result)
    (cons 'set-face-attribute result)))

(defmacro colors! (&rest color-config)
  (declare (indent defun))
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
	(font         (plist-get color-config :font))
	(mode-line    (plist-get color-config :mode-line))
	(mode-line-inactive (plist-get color-config :mode-line-inactive))
	(transparency (plist-get color-config :transparency)))
    (when background
      (if (display-graphic-p)
	  (push `(set-background-color ,background) result)
	(push `(set-face-background 'default ,background) result)))
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
    (when mode-line
      (push (generate-face-attribute :mode-line mode-line) result))
    (when mode-line-inactive
      (push (generate-face-attribute :mode-line-inactive mode-line-inactive)
	    result))
    (when fringe
      (push (generate-face-attribute :fringe fringe) result))
    (when transparency
      (let ((letter (car transparency))
	    (num1   (cadr transparency))
	    (num2   (caddr transparency)))
	(push `(set-frame-parameter (selected-frame) ,letter '(,num1 ,num2)) result)))
    (when font
      (push (generate-face-attribute :font font) result))
    (cons 'progn result)))

;;; Mode Line
(defun parse-mode-line-spec (mode-line-spec)
  (let ((text      (plist-get mode-line-spec :text))
	(color     (plist-get mode-line-spec :color))
	(condition (plist-get mode-line-spec :condition))
	(result    nil))
    (when condition
      (setq text
	    `(if ,condition
		 ,text
	       "")))
    (if (stringp text)
	(if (not color)
	    (setq result text)
	  (setq result
		`(:eval
		  (propertize
		   ,text
		   'face '(:foreground ,color)))))
      (if (not color)
	  (setq result `(:eval ,text))
	(setq result
	      `(:eval
		(propertize
		 ,text
		 'face '(:foreground ,color))))))
    result))

(defmacro mode-line! (&rest mode-line-specs)
  (declare (indent defun))
  (let ((result '()))
    (dolist (spec mode-line-specs)
      (push `',(parse-mode-line-spec spec) result))
    (setq result (reverse result))
    (push 'list result)
    (setq result
	  `(setq-default mode-line-format
			 ,result))
    result))
