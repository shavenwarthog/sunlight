;; acheck.el -- asynchronously check code in buffer, highlight issues
;;
;; Pylint 0.21.1
;; pylint --errors-only -fparseable -iy sunfudge.py 

;; (cadr (car (car elint-buffer-forms)))
;;    (if elint-top-form-logged

(defvar acheck-sourcebuf nil "Buffer containing source code")
(defvar acheck-proc nil "acheck process")
(defvar acheck-workfile-path nil "temporary copy of source code")
;; XX buffer local
(defvar acheck-message-count nil "x")
;; XX buffer local

;; :::::::::::::::::::::::::::::::::::::::::::::::::: EMACS LISP

(when nil
  (defadvice elint-log-message (after acheck-elint-log-message
				      (errstr))
    "Get form name, location, and error message."
    (let* ((form (elint-top-form-form elint-top-form))
	   (top (car form))
	   (pos (elint-top-form-pos elint-top-form)))
      (message "form: %s %s %s" form top pos)))
  
  (ad-activate 'elint-log-message)
  
  (defun jmc-test ()
    (elint-current-buffer)))


;; :::::::::::::::::::::::::::::::::::::::::::::::::: PYLINT

(defface acheck-pylint-error
  '((t :underline "red2"))
  "pylint error")
(defface acheck-pylint-warning
  '((t :underline "IndianRed"))
  "pylint warning")

; (copy-face 'hi-red-b 'acheck-pylint-error)

;; (defface acheck-pylint-message
;;   '((t :foreground "gray60"))
;;   "pylint message, to right of underlined code")
;;;     (overlay-put ov 'after-string 
;;; 		 (propertize (concat "  " (match-string 5 line))
;;; 			     'face 'acheck-pylint-message))))

;; "pylint --errors-only -fparseable -iy %s"

(defun acheck-pylint-command (workfile-path)
  (split-string 
   (format "pylint --disable=c,i,r -fparseable -iy %s"
;;   (format "pylint -e -fparseable -iy %s"
	   workfile-path)))
  
(defun acheck-pylint-parse (line)
  (string-match (concat 
		 "\\(.+?\\):\\([0-9]+\\):" ;; filename/1 : lineno/2 :
		 " \\[\\(.\\)"		  ;; error code/3
		 "\\([0-9]*\\)"		  ;; error id/4 (optional)
		 "[ ,]*\\(.+?\\)\\]" ;; objname/5 (optional)
		 " \\(.+\\)"	     ;; message/6
		 )
		line))

;; XX: name: filter?
(defun acheck-pylint-good-p (line)
  (not (string-match-p ".+assert" line)))

;; (when nil
;;   (defun acheck-pylint-annotate (ov line)
;;     (overlay-put ov 'face 
;; 		 (cond ((string= "W" (match-string 5 line))
;; 		     'acheck-pylint-warning)
;;     (let ((message (match-string 5 line)))
;;       (if (string-match ".*'\\(.+?\\)'" message)
;; 	  (overlay-put ov 'help-echo 
;; 		       (overlay-put ov 'help-echo message))))))
		       
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
	
 (defun acheck-pylint-annotate (ov line)
   (defalias 'errcode= (apply-partially 'string= (match-string 3 line)))
   ;; (message "beer: %s %s" (errcode= "E") (match-string 3 line))
   (overlay-put 
    ov 'face
    (if (errcode= "W") 
	'acheck-pylint-warning
      'acheck-pylint-error))
   (let ((note (format "%s (%s%s)" (match-string 6 line)
		       (match-string 3 line)
		       (match-string 4 line))))
     (overlay-put ov 'help-echo note)))


(defun jmc-test () 
  (let ((line "sunfudge.py:2: [E0602, Fake] Undefined variable 'fudge'"))
    (acheck-parse line)
    (message "woo: %s" (match-string 4 line))))



;; ::::::::::::::::::::::::::::::::::::::::::::::::::

(defun acheck-write-workfile ()
  ;; current directory, to preserve imports
  ;; XX suffix
  (setq acheck-workfile-path (concat (make-temp-name "ac_") ".py"))
  (write-region nil nil acheck-workfile-path :visit -1))

(defun acheck-delete-workfile ()
  (when (file-exists-p acheck-workfile-path)
      (delete-file acheck-workfile-path)
      (setq acheck-workfile-path nil)))

(defun acheck-check ()
  (interactive)
  (setq acheck-sourcebuf (current-buffer))
  (acheck-write-workfile)
  (acheck-remove-overlays)
  (setq acheck-proc 
	(apply 
	 'start-process 
	 "acheck" "*pylint*" 
	 (acheck-pylint-command acheck-workfile-path)))
  (set-process-filter acheck-proc 'acheck-filter)
  (set-process-sentinel acheck-proc 'acheck-sentinel))

(defun acheck-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	;; Insert the text, advancing the process marker.
	(goto-char (process-mark proc))
	(insert (concat (format-time-string "\n%H:%M:%S\n")
			string))
	(acheck-parsebuf string)
	(set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun acheck-sentinel (proc string)
  (acheck-delete-workfile))

(defun acheck-remove-overlays ()
  (interactive)
  (remove-overlays nil nil 'acheck t))

(defun acheck-make-overlay (lineno)
  (save-excursion
    (goto-line (string-to-number lineno))
    (skip-chars-forward "[:blank:]")
    (make-overlay (point) (line-end-position))))

(defun acheck-parse (line)
  (when (and (acheck-pylint-parse line)
	     (acheck-pylint-good-p line))
    ;; (setq acheck-message-count (1+ acheck-message-count))
    (save-excursion
      (set-buffer acheck-sourcebuf)
      (let ((ov (acheck-make-overlay (match-string 2 line))))
	(overlay-put ov 'acheck t)
	(acheck-pylint-annotate ov line)))))

(defun acheck-parsebuf (bufstr)
  ;; (setq acheck-message-count 0)
  (mapc 'acheck-parse (split-string bufstr "\n")))
;;   (message "acheck: %s messages" acheck-message-count))


;; XXX: interface with simple.el:next-error-function

(defun acheck-next-error ()
  (interactive)
  (let ((nextov (next-overlay-change (line-end-position))))
    (if (< nextov (point-max))
	(progn
	  (goto-char nextov)
	  (message "acheck: %s" (flynote-current-message)))
      (message "acheck: end"))))

;; (defun acheck-next-error2 ()

(global-set-key (kbd "<kp-insert>") 'acheck-next-error)
(global-set-key (kbd "C-<kp-insert>") 'next-error)
  




(defun jmc-test ()
  (find-file-other-window "sunfudge.py")
  (acheck-check))
  ;; (acheck-parse "sunfudge.py:2: [E, Fake] Undefined variable 'fudge'"))
;; (jmc-retest)
  
	     


; (modify-face 'acheck-master "foreground" nil)
;; (overlay-put (make-overlay (- (point-max) 50) (point-max))
;; 	     'face '(:underline "red2"))

 ;; (overlay-put (make-overlay (- (point-max) 50) (point-max))
 ;; 	     'face 'acheck-pylint)


;; (define-derived-mode acheck-display-mode python-mode "Ac"
;;   (buffer-disable-undo)
;;   (setq next-error-function 'acheck-next-error)
;;   (setq next-error-last-buffer (current-buffer))
;;   (define-key acheck-display-mode-map "\C-m" 'acheck-goto-expect)
;;   (define-key acheck-display-mode-map "\C-c\C-c" 'acheck-goto-expect))

;; (define-minor-mode acheck
;;   "Check source"
;;   nil " Ac")

(provide 'acheck)
