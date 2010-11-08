;; piemacs.el -- run process, eval lisp expressions

(defvar piemacs-sourcebuf nil "Buffer containing source code")
(defvar piemacs-proc nil "piemacs process")
(defvar piemacs-workfile-path nil "temporary copy of source code")
;; XX buffer local

;; :::::::::::::::::::::::::::::::::::::::::::::::::: BASE

(defun piemacs-write-workfile ()
  ;; current directory, to preserve imports
  ;; XX suffix
  (setq piemacs-workfile-path (concat (make-temp-name "pie_") ".py"))
  (write-region nil nil piemacs-workfile-path :visit -1))

(defun piemacs-delete-workfile ()
  (when (file-exists-p piemacs-workfile-path)
      (delete-file piemacs-workfile-path)
      (setq piemacs-workfile-path nil)))


;; :::::::::::::::::::::::::::::::::::::::::::::::::: INTERACTIVE

(defun piemacs-command (path)
  (list "date" "+(message \"it is now %c\")"))


(defun piemacs-check ()
  (interactive)
  (setq piemacs-sourcebuf (current-buffer))
  (piemacs-write-workfile)
  (piemacs-remove-overlays)
  (let ((cmd (piemacs-command piemacs-workfile-path)))
    (setq piemacs-proc 
	  (apply 'start-process "piemacs" (format "*piemacs: %s*" (car cmd)) (car cmd) (cdr cmd)))
    (with-current-buffer (process-buffer piemacs-proc)
      (insert (format "command: %s\n\n" cmd))
      (goto-char (process-mark piemacs-proc))))
  (set-process-filter piemacs-proc 'piemacs-filter)
  (set-process-sentinel piemacs-proc 'piemacs-sentinel))

(defun piemacs-remove-overlays ()
  (interactive)
  (remove-overlays nil nil 'piemacs t))


;; XX: interface with simple.el:next-error-function

(defun piemacs-next-error ()
  (interactive)
  (let ((nextov (next-overlay-change (line-end-position))))
    (if (< nextov (point-max))
	(progn
	  (goto-char nextov)
	  (message "piemacs: %s" (flynote-current-message)))
      (message "piemacs: end"))))


;; :::::::::::::::::::::::::::::::::::::::::::::::::: PROCESS/PARSE

(defun piemacs-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	(goto-char (process-mark proc))
	(insert (concat (format-time-string "\n%H:%M:%S\n")
			string))
	(piemacs-parsebuf string)
	(set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun piemacs-sentinel (proc string)
  (piemacs-delete-workfile))

(defun piemacs-parse (line)
  (save-excursion
    (set-buffer piemacs-sourcebuf)
    (condition-case nil
	(eval (car (read-from-string line)))
      (error nil)))) 

(defun piemacs-parsebuf (bufstr)
  (mapc 'piemacs-parse (split-string bufstr "\n")))


;; :::::::::::::::::::::::::::::::::::::::::::::::::: OVERLAY

(require 'cl)

(defun piemacs-make-ov-lineno (lineno)
  (save-excursion
    (goto-line lineno)
    (skip-chars-forward "[:blank:]")
    (make-overlay (point) (line-end-position))))

(defun* piemacs-ov (&key lineno message face linerange)
  (let ((ov (piemacs-make-ov-lineno lineno)))
    (overlay-put ov 'piemacs t)
    (overlay-put ov 'face (or face 'piemacs-pylint-error))
    (overlay-put ov 'help-echo message)))

(when nil
  (defun piemacs-command (path)
    "Highlight the current line, add important hover message."
    (list "echo" "(piemacs-ov :message \"beer\" :face 'highlight)")))

;; :::::::::::::::::::::::::::::::::::::::::::::::::: NOSETESTS / COVERAGE
;; (piemacs-nosetest "test_callname_shouldskip" 'piemacs-face-okay)
;; (piemacs-nosetest "test_enum_pos" 'piemacs-face-okay)
;; (piemacs-coverage-missing 20 20 32 32 42 42 45 59 62 72 75 112 116 127 131 143 146 146)
(when nil
  (mapc (apply-partially 'piemacs-ov :message "woo" :lineno)
	(list 109)))
  (mapc (apply-partially 'piemacs-ov :message "beer" :linerange)
	(list (list 112 114)))

(defface piemacs-coverage-missing
  '((t :foreground "gray80" :underline "gray"))
  "coverage missing for this line")

;; standard Emacs faces:
;; default	  fixed-pitch	    isearch	      secondary-selection
;; bold		  variable-pitch    query-replace     trailing-whitespace
;; italic	  shadow	    lazy-highlight    nobreak-space
;; bold-italic	  highlight	    region	      escape-glyph
;; underline

;; (defun piemacs-coverage-missing (lines)
;;   (let ((arg2))
;;     (dolist (arg1 lines)
;;       (setq line1 
  
;;   )

;; :::::::::::::::::::::::::::::::::::::::::::::::::: PYLINT

(defface piemacs-pylint-error
  '((t :underline "red2"))
  "pylint error")
(defface piemacs-pylint-warning
  '((t :underline "IndianRed"))
  "pylint warning")

(defun piemacs-pylint-command (workpath)
  (list "python" "./ppylint.py" workpath))

	


(global-set-key (kbd "<kp-insert>") 'piemacs-next-error)
(global-set-key (kbd "C-<kp-insert>") 'next-error)


(provide 'piemacs)

(defun jmc-test () (piemacs-check))