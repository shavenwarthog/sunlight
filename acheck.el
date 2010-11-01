;; acheck.el -- asynchronously check code in buffer, highlight issues

;; Pylint 0.21.1
;; pylint --errors-only -fparseable -iy sunfudge.py 

;; (cadr (car (car elint-buffer-forms)))
;;    (if elint-top-form-logged

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
; (copy-face 'hi-red-b 'acheck-pylint-error)

(defun acheck-pylint-parse (line)
  (string-match (concat 
		 "\\(.+?\\):\\([0-9]+\\):" ;; filename/1 : lineno/2 :
		 " \\[\\(.\\)"		  ;; error code/3
		 "[ ,]*\\(.+?\\)\\]" ;; objname/4 (optional)
		 " \\(.+\\)"	     ;; message/5
		 )
		line))

(defun acheck-pylint-annotate (ov line)
  (overlay-put ov 'face 'acheck-pylint-error)
  (overlay-put ov 'help-echo (match-string 5 line)))


;; ::::::::::::::::::::::::::::::::::::::::::::::::::

(defun acheck-check ()
  (interactive)
  (setq acheck-proc (start-process "acheck" "*pylint*" "head" "-20" "pylint.out"))
  (set-process-filter acheck-proc 'acheck-filter))

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

(defun acheck-remove-overlays ()
  (interactive)
  (remove-overlays nil nil 'acheck t))

(defun acheck-make-overlay (lineno)
  (save-excursion
    (goto-line (string-to-number lineno))
    (skip-chars-forward "[:blank:]")
    (make-overlay (point) (line-end-position))))

(defun acheck-parse (line)
  (when (acheck-pylint-parse line)
    (save-excursion
      (set-buffer (get-buffer "sunfudge.py"))
      (let ((ov (acheck-make-overlay (match-string 2 line))))
	(overlay-put ov 'acheck t)
	(acheck-pylint-annotate ov line)))))

(defun acheck-parsebuf (bufstr)
  (mapc 'acheck-parse (split-string bufstr "\n")))

(defun jmc-test ()
  (find-file-other-window "sunfudge.py")
  (acheck-remove-overlays)
  (acheck-check))
  ;; (acheck-parse "sunfudge.py:2: [E, Fake] Undefined variable 'fudge'"))
;; (jmc-retest)
  
	     


; (modify-face 'acheck-master "foreground" nil)
;; (overlay-put (make-overlay (- (point-max) 50) (point-max))
;; 	     'face '(:underline "red2"))

 ;; (overlay-put (make-overlay (- (point-max) 50) (point-max))
 ;; 	     'face 'acheck-pylint)



