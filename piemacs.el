;; piemacs.el -- run process, eval lisp expressions

(require 'cl)

(defvar piemacs-load-path nil "load-path for Piemacs plugins")
;; XX: add location of piemacs.el 

;; XX buffer local:
(defvar piemacs-command-function nil "Function returning shell command")
(defvar piemacs-check-function nil "Function that checks current buffer")
(defvar piemacs-sourcebuf nil "Buffer containing source code")
(defvar piemacs-proc nil "piemacs process")
(defvar piemacs-workfile-path nil "temporary copy of source code")
(defvar piemacs-timer nil "timer")
(defvar piemacs-timer-secs 0.1 "timer idle timout, in seconds")


;; :::::::::::::::::::::::::::::::::::::::::::::::::: FACES

(defface piemacs-error
  '((t :underline "red2"))
  "error")

;; :::::::::::::::::::::::::::::::::::::::::::::::::: HELPERS

(defun piemacs-idle-callback (last-modified-tick)
  "Run check if source buffer modified since last-modified-tick.
Then, start another timer, with new modification time."
  (when (> (buffer-modified-tick piemacs-sourcebuf) last-modified-tick)
    (cancel-timer piemacs-timer)
    (piemacs-check)
    (piemacs-restart-timer)))

(defun piemacs-stop-timer ()
  (when piemacs-timer
    (cancel-timer piemacs-timer)))

(defun piemacs-restart-timer ()    
  (piemacs-stop-timer)
  (setq piemacs-timer (run-with-idle-timer 
		       piemacs-timer-secs t 'piemacs-idle-callback 
		       (buffer-modified-tick piemacs-sourcebuf))))

(defun piemacs-write-workfile ()
  ;; current directory, to preserve imports
  ;; XX suffix
  (setq piemacs-workfile-path (concat (make-temp-name "pie_") ".py"))
  (write-region nil nil piemacs-workfile-path :visit -1))

(defun piemacs-delete-workfile ()
  (when (and piemacs-workfile-path (file-exists-p piemacs-workfile-path))
      (delete-file piemacs-workfile-path)
      (setq piemacs-workfile-path nil)))

(defun piemacs-locate (name):
  (locate-library name t piemacs-load-path))

(defun piemacs-status (msg)
  (message "piemacs: %s" msg))

;; XX
(defun piemacs-checkable ()
  (eq 'python-mode major-mode))


;; :::::::::::::::::::::::::::::::::::::::::::::::::: INTERACTIVE

(defun piemacs-check ()
  "Check and annotate current buffer."
  (interactive)
  (if piemacs-check-function
      (funcall piemacs-check-function)
    (piemacs-status "no check function set")))

(defun piemacs-remove-overlays ()
  (interactive)
  (remove-overlays nil nil 'piemacs t))


;; XX: interface with simple.el:next-error-function

(when nil
(defun piemacs-next-error ()
  BLAM
  (interactive)
  (let ((nextov (next-overlay-change (line-end-position))))
    (if (< nextov (point-max))
	(progn
	  (goto-char nextov)
	  (message "piemacs: %s" (flynote-current-message)))
      (message "piemacs: end")))))


;; :::::::::::::::::::::::::::::::::::::::::::::::::: PROC START/STOP

(defun piemacs-check-sync ()
  "Sync: call (piemacs-command-function workfile-path), proc dies after check"
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

(defun piemacs-start ()
  "Async: start proc server, each check sends buffer-contents, proc remains."
  (let* ((cmd (funcall piemacs-command-function))
	 (bufname (if cmd (format "*piemacs: %s*" (car cmd)))))
    (piemacs-restart-timer)
    (when cmd
      (setq piemacs-proc 
	    (apply 'start-process-shell-command "piemacs" bufname (car cmd) (cdr cmd)))
      (with-current-buffer (process-buffer piemacs-proc)
	(insert (format "command: %s\n\n" cmd))
	(goto-char (process-mark piemacs-proc))))
    (set-process-filter piemacs-proc 'piemacs-filter)
    (set-process-sentinel piemacs-proc 'piemacs-sentinel)))

(defun piemacs-stopped ()
  (or (null piemacs-proc) 
      (> (process-exit-status piemacs-proc) 0)))

(defun piemacs-restart ()
  (interactive)
  (piemacs-stop)
  ;; (when (piemacs-stopped)
  (piemacs-start))

(defun piemacs-stop ()
  (interactive)
  (piemacs-stop-timer)
  (when (not (piemacs-stopped))
    (delete-process piemacs-proc)))

;; XX: assumes current buffer
(defun piemacs-send-buffer ()
  (with-current-buffer piemacs-sourcebuf
    (process-send-string piemacs-proc 
			 (format ">%d\t" (- (point-max) (point-min))))
    (process-send-region piemacs-proc 
			 (point-min) (point-max))))

(defun piemacs-check-with-server ()
  (when (piemacs-checkable)
    (setq piemacs-sourcebuf (current-buffer))
    (piemacs-remove-overlays)
    (when (piemacs-stopped)
      (piemacs-start))
    (piemacs-send-buffer)))


;; :::::::::::::::::::::::::::::::::::::::::::::::::: PROC FILTER/PARSE

(defun piemacs-log (proc string &optional func)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	(goto-char (process-mark proc))
	(insert (concat (format-time-string "\n%H:%M:%S\n")
			string))
	(when func 
	  (funcall func string))
	(set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun piemacs-filter (proc string)
  (piemacs-log proc string 'piemacs-parsebuf))

(defun piemacs-sentinel (proc string)
  ;; (defun _log (fmt &rest args)
  ;;   (piemacs-log proc (format "piemacs %s %s" "xx" (apply 'format fmt args))))
  ;; (_log "beer"))
    
  (piemacs-log proc (format "sentinel: %s\n" string))
  (when (eq (process-status proc) 'exit)
    (let ((status (process-exit-status proc)))
      (if (= 0 status)
	  (piemacs-log proc (format "exit: okay\n"))
	(piemacs-log proc (format "exit: status=%d\n" status))))
    (piemacs-delete-workfile)))

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
    (overlay-put ov 'face (or face 'piemacs-error))
    (when message
      (overlay-put ov 'help-echo message))))

(defun* piemacs-ovs (&key message face lineranges)
  (while lineranges
    (let* ((lstart (pop lineranges))
	   (lend (pop lineranges)))
      (piemacs-ov :message message :face face :linerange (list lstart lend)))))

(defun* piemacs-ov-pos (&key lineno col message face)
  (save-excursion
    (goto-line lineno)
    (let* ((pos (1- (+ (point) col)))
	   (ov (piemacs-make-overlay pos (1+ pos))))
      (overlay-put ov 'face (or face 'piemacs-error))
      (when message
	(overlay-put ov 'help-echo message)))))


;; :::::::::::::::::::::::::::::::::::::::::::::::::: PLUGIN: PYLINT

(copy-face 'piemacs-error 'piemacs-pylint-error)
(defface piemacs-pylint-warning
  '((t :underline "IndianRed"))
  "pylint warning")

(defvar piemacs-pylint-command "ppylint.py" "pylint command")

(defun piemacs-pylint-command (source-path)
  (let ((path (piemacs-locate piemacs-pylint-command)))
    (if (file-exists-p path)
	(list path source-path)
      (error "piemacs: pylint command %s not found; check load-path" 
	     piemacs-pylint-command))))

(defun piemacs-set-pylint ()
  (setq piemacs-command-function 'piemacs-pylint-command
	piemacs-check-function 'piemacs-check-sync))


;; :::::::::::::::::::::::::::::::::::::::::::::::::: PLUGIN: FASTCHECK
;; highlight Python syntax errors
;; (server)

(defface piemacs-fastcheck
  '((t :background "firebrick4"))
  ".")

;; XXX
(defun piemacs-fastcheck-command ()
  (list "python2.6" "./fastcheck.py" "--server"))

(defun piemacs-set-fastcheck ()
  (setq piemacs-command-function 'piemacs-fastcheck-command
	piemacs-check-function 'piemacs-check-with-server))


;; :::::::::::::::::::::::::::::::::::::::::::::::::: PLUGIN: VCAGE
;; using version control, older code is smaller

(defface vcage-old-face
  '((t :height 0.90))
  ".")
(defface vcage-oldest-face
  '((t :height 0.75))
  ".")
;; (set-face-attribute 'vcage-old-face nil :height 0.90)

(defun piemacs-set-vcage ()
  (setq piemacs-command-function 
	(lambda (path) (list "python2.6" "./vcage.py" path))
	piemacs-check-function 'piemacs-check-with-server))



;; :::::::::::::::::::::::::::::::::::::::::::::::::: PLUGIN: NOSETESTS / COVERAGE


(defface coverage-missing
  '((t :box "gray80")) ;;:foreground "gray80" :underline "gray"))
  "coverage missing for this line")

(defun nosetests-command (path)
  (concat (piemacs-locate "pnosetests.py") path))
;;	  (split-string "python2.6 ./pnosetests.py test_xref.py"))


;; :::::::::::::::::::::::::::::::::::::::::::::::::: PLUGIN: PIEMACS DEBUG

;; XXXX:
(defun piemacs-set-debug ()
  (setq piemacs-command-function
	'(lambda (_)
	  (list "date" "'+(message \"it is now %c\")'"))
	piemacs-check-function 'piemacs-check-sync))

;; :::::::::::::::::::::::::::::::::::::::::::::::::: PLUGIN: EMACS LISP
;; adapted from http://www.emacswiki.org/emacs/FlymakeElisp
;; cf: (error-message-string)

(defun piemacs-elisp-check ()
  (piemacs-remove-overlays)
  (save-excursion
    (condition-case errinfo
	(scan-sexps (point-min) (point-max))
      (scan-error
       (let ((errmsg (error-message-string (list (car errinfo)))))
	 (piemacs-ov
	  :lineno (line-number-at-pos (nth 2 errinfo))
	  :face 'piemacs-pylint-error
	  :message errmsg)
	 (piemacs-status errmsg))))))

;; XXXX: wrong
(defun piemacs-set-elisp ()
  (setq piemacs-command-function '(lambda () nil)
	piemacs-check-function 'piemacs-elisp-check))
      

;; :::::::::::::::::::::::::::::::::::::::::::::::::: 	


(when nil
  (global-set-key (kbd "<kp-insert>") 'piemacs-next-error)
  (global-set-key (kbd "C-<kp-insert>") 'next-error))
  
(provide 'piemacs)


;; :::::::::::::::::::::::::::::::::::::::::::::::::: HISTORICAL



(when nil
  (defun piemacs-echo-command (path)
    "Highlight the current line, add important hover message."
    (list "echo" "(piemacs-ov :message \"beer\" :face 'highlight)")))

;; standard Emacs faces:
;; default	  fixed-pitch	    isearch	      secondary-selection
;; bold		  variable-pitch    query-replace     trailing-whitespace
;; italic	  shadow	    lazy-highlight    nobreak-space
;; bold-italic	  highlight	    region	      escape-glyph
;; underline


