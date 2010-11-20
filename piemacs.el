;; piemacs.el -- run process, eval lisp expressions

(require 'cl)
(add-to-list 'load-path "~/src/sunlight") ;XXX

(defvar piemacs-command-function nil "Function returning shell command")
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

(when nil
  (defun piemacs-date-command (path)
    (list "date" "+(message \"it is now %c\")")))

(defun piemacs-checkable ()
  (eq 'python-mode major-mode))

(defun piemacs-check ()
  (interactive)
  (when (piemacs-checkable)
    (setq piemacs-sourcebuf (current-buffer))
    (piemacs-write-workfile)
    (piemacs-remove-overlays)
    (let* ((cmd (funcall piemacs-command-function piemacs-workfile-path))
	   (bufname (format "*piemacs: %s*" (car cmd))))
      (setq piemacs-proc 
	    (apply 'start-process-shell-command "piemacs" bufname (car cmd) (cdr cmd)))
      (with-current-buffer (process-buffer piemacs-proc)
	(insert (format "command: %s\n\n" cmd))
	(goto-char (process-mark piemacs-proc))))
    (set-process-filter piemacs-proc 'piemacs-filter)
    (set-process-sentinel piemacs-proc 'piemacs-sentinel)))

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

(defun piemacs-make-overlay (start end)
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'piemacs t)
    ov))

(defun piemacs-make-ov-lineno (lineno)
  (save-excursion
    (goto-line lineno)
    (skip-chars-forward "[:blank:]")
    (piemacs-make-overlay (point) (line-end-position))))

(defun piemacs-make-ov-linerange (linerange)
  "Highlight line range, inclusive."
  (save-excursion
    (let ((lstart (car linerange))
	  (lend (cadr linerange)))
      (goto-line lstart)
      (piemacs-make-overlay (point) (line-end-position (1+ (- lend lstart)))))))

(defun* piemacs-ov (&key lineno message face linerange)
  (let ((ov (cond
	     (lineno (piemacs-make-ov-lineno lineno))
	     (linerange (piemacs-make-ov-linerange linerange))
	     (t (error "Unknown region")))))
    (overlay-put ov 'face (or face 'piemacs-pylint-error))
    (overlay-put ov 'help-echo message)))

(defun* piemacs-ovs (&key message face lineranges)
  (while lineranges
    (let* ((lstart (pop lineranges))
	   (lend (pop lineranges)))
      (piemacs-ov :message message :face face :linerange (list lstart lend)))))


(when nil
  (defun piemacs-echo-command (path)
    "Highlight the current line, add important hover message."
    (list "echo" "(piemacs-ov :message \"beer\" :face 'highlight)")))

;; :::::::::::::::::::::::::::::::::::::::::::::::::: FASTCHECK
;; highlight Python syntax errors

(defface fastcheck-face
  '((t :box t))
  ".")

(defun fastcheck-command (workpath)
  (list "python2.6" "./fastcheck.py" "<" workpath))

;; (funcall piemacs-command "beer")

(defun fastcheck-err (lineno errpos message)
  (message "fastcheck: +%s '%s'" lineno message)
  (piemacs-ov :lineno lineno :message message :face 'fastcheck-face))


(defun piemacs-set-fastcheck ()
  (setq piemacs-command-function 'fastcheck-command))

;; :::::::::::::::::::::::::::::::::::::::::::::::::: NOSETESTS / COVERAGE

;; (piemacs-nosetest "test_callname_shouldskip" 'piemacs-face-okay)
;; (piemacs-nosetest "test_enum_pos" 'piemacs-face-okay)
;; (piemacs-coverage-missing 20 20 32 32 42 42 45 59 62 72 75 112 116 127 131 143 146 146)
(when nil
  (mapc (apply-partially 'piemacs-ov :message "woo" :lineno)
	(list 109)))
  (mapc (apply-partially 'piemacs-ov :message "beer" :linerange)
	(list (list 112 114)))

(defun piemacs-command (path)
  (concat (piemacs-locate "pnosetests.py") path))
;;	  (split-string "python2.6 ./pnosetests.py test_xref.py"))
(defun piemacs-command (path)
  (list (piemacs-locate "ppylint.py") path))

(defface piemacs-coverage-missing
  '((t :box "gray80")) ;;:foreground "gray80" :underline "gray"))
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

;; :::::::::::::::::::::::::::::::::::::::::::::::::: HELPERS

(defun piemacs-locate (name):
  (locate-library name t))		; look on load-path X

(defun piemacs-status (msg)
  (message "piemacs: %s" msg))

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
