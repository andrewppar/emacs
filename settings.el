(setq x-select-enable-clipboard t)

(add-hook 'write-file-functions 'delete-trailing-whitespace)
(setq lisp-indent-function 'common-lisp-indent-function)

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq user-mail-address  "anparisi@cisco.com")
