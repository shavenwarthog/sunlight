;; atest.el -- asynchronously mark tests as ok/fail/error

(defvar acheck-sourcebuf nil "Buffer containing source code")
(defvar atest-proc nil "acheck process")
(defvar atest-workfile-path nil "temporary copy of source code")
;; XX buffer local
(defvar atest-message-count nil "x")
;; XX buffer local



(defface atest-error
  '((t :underline "red2"))
  "error")
(defface atest-fail
  '((t :underline "IndianRed"))
  " warning")

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
	
 (defun atest-pylint-annotate (ov line)
   (defalias 'errcode= (apply-partially 'string= (match-string 3 line)))
   ;; (message "beer: %s %s" (errcode= "E") (match-string 3 line))
   (overlay-put 
    ov 'face
    (if (errcode= "W") 
	'atest-pylint-warning
      'atest-pylint-error))
   (let ((note (format "%s (%s%s)" (match-string 6 line)
		       (match-string 3 line)
		       (match-string 4 line))))
     (overlay-put ov 'help-echo note)))



(defun atest-check ()
  (interactive)
  (setq atest-sourcebuf (current-buffer))
  (atest-write-workfile)
  (atest-remove-overlays)
  (setq atest-proc 
	(apply 
	 'start-process 
	 "acheck" "*pylint*" 
	 (atest-pylint-command atest-workfile-path)))
  (set-process-filter atest-proc 'atest-filter)
  (set-process-sentinel atest-proc 'atest-sentinel))

(defun atest-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	;; Insert the text, advancing the process marker.
	(goto-char (process-mark proc))
	(insert (concat (format-time-string "\n%H:%M:%S\n")
			string))
	(atest-parsebuf string)
	(set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun atest-sentinel (proc string)
  (atest-delete-workfile))

(defun atest-remove-overlays ()
  (interactive)
  (remove-overlays nil nil 'acheck t))

(defun atest-make-overlay (lineno)
  (save-excursion
    (goto-line (string-to-number lineno))
    (skip-chars-forward "[:blank:]")
    (make-overlay (point) (line-end-position))))

(defun atest-parse (line)
  (when (and (atest-pylint-parse line)
	     (atest-pylint-good-p line))
    ;; (setq atest-message-count (1+ atest-message-count))
    (save-excursion
      (set-buffer atest-sourcebuf)
      (let ((ov (atest-make-overlay (match-string 2 line))))
	(overlay-put ov 'acheck t)
	(atest-pylint-annotate ov line)))))

(defvar atest-state-alist nil ".")

(defun atest-parsebuf (bufstr)
  (let ((start nil)
	(setq atest-state-alist nil))	;XXX
    (when (setq start 			;XXX
		 (string-match "(.*\\.\\(.+\\)) ... \\(.+\\)"
			       bufstr start))
      (message "atest: %s %s" 
	       (match-string 1 bufstr) (match-string 2 bufstr))
      (push (list (match-string 1 bufstr)
		  (match-string 2 bufstr))
	    atest-state-alist))))


(defun jmc-test () (atest-parsebuf "beer (a.b.BeerTest) ... ok"))
 

(provide 'atest)


