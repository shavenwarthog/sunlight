;; piemacs.el -- asynchronously check code in buffer, highlight issues

(defvar piemacs-sourcebuf nil "Buffer containing source code")
(defvar piemacs-proc nil "piemacs process")
(defvar piemacs-workfile-path nil "temporary copy of source code")
;; XX buffer local

;; :::::::::::::::::::::::::::::::::::::::::::::::::: PYLINT

(defface piemacs-pylint-error
  '((t :underline "red2"))
  "pylint error")
(defface piemacs-pylint-warning
  '((t :underline "IndianRed"))
  "pylint warning")

;;;; Compatibility
(unless (fboundp 'apply-partially)
  (defun apply-partially (fun &rest args)
    "Return a function that is a partial application of FUN to ARGS.
ARGS is a list of the first N arguments to pass to FUN.
The result is a new function which does the same as FUN, except that
the first N arguments are fixed at the values with which this function
was called."
    (lexical-let ((fun fun) (args1 args))
      (lambda (&rest args2) (apply fun (append args1 args2))))))
	
;; :::::::::::::::::::::::::::::::::::::::::::::::::: HELPERS

 (defun piemacs-ov (lineno msg)
   (let ((ov (piemacs-make-overlay lineno)))
     (overlay-put ov 'piemacs t)
     (overlay-put ov 'face 'piemacs-pylint-error)
     (overlay-put ov 'help-echo msg)))

;; ::::::::::::::::::::::::::::::::::::::::::::::::::

(defun piemacs-write-workfile ()
  ;; current directory, to preserve imports
  ;; XX suffix
  (setq piemacs-workfile-path (concat (make-temp-name "pie_") ".py"))
  (write-region nil nil piemacs-workfile-path :visit -1))

(defun piemacs-delete-workfile ()
  (when (file-exists-p piemacs-workfile-path)
      (delete-file piemacs-workfile-path)
      (setq piemacs-workfile-path nil)))

(defun piemacs-pylint-command (workpath)
  (list "python" "./ppylint.py" workpath))

;; :::::::::::::::::::::::::::::::::::::::::::::::::: INTERACTIVE

(defun piemacs-check ()
  (interactive)
  (setq piemacs-sourcebuf (current-buffer))
  (piemacs-write-workfile)
  (piemacs-remove-overlays)
  (setq piemacs-proc 
	(apply 
	 'start-process 
	 "piemacs" "*pylint*" 
	 (piemacs-pylint-command piemacs-workfile-path)))
  (set-process-filter piemacs-proc 'piemacs-filter)
  (set-process-sentinel piemacs-proc 'piemacs-sentinel))

(defun piemacs-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	;; Insert the text, advancing the process marker.
	(goto-char (process-mark proc))
	(insert (concat (format-time-string "\n%H:%M:%S\n")
			string))
	(piemacs-parsebuf string)
	(set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun piemacs-sentinel (proc string)
  (piemacs-delete-workfile))

(defun piemacs-remove-overlays ()
  (interactive)
  (remove-overlays nil nil 'piemacs t))

(defun piemacs-make-overlay (lineno)
  (save-excursion
    (goto-line lineno)
    (skip-chars-forward "[:blank:]")
    (make-overlay (point) (line-end-position))))

(defun piemacs-parse (line)
  (save-excursion
    (set-buffer piemacs-sourcebuf)
    (condition-case nil
	(eval (car (read-from-string line)))
      (error (message "beer: %s" line)))))

(defun piemacs-parsebuf (bufstr)
  (mapc 'piemacs-parse (split-string bufstr "\n")))


;; XX: interface with simple.el:next-error-function

(defun piemacs-next-error ()
  (interactive)
  (let ((nextov (next-overlay-change (line-end-position))))
    (if (< nextov (point-max))
	(progn
	  (goto-char nextov)
	  (message "piemacs: %s" (flynote-current-message)))
      (message "piemacs: end"))))


(global-set-key (kbd "<kp-insert>") 'piemacs-next-error)
(global-set-key (kbd "C-<kp-insert>") 'next-error)


(provide 'piemacs)
