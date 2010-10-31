(defface acheck-pylint-error
  '((t :underline "red2"))
  "pylint error")

(defun acheck-check ()
  (interactive)
  (setq acheck-proc (start-process "acheck" "*pylint*" "head" "-2" "pylint.out"))
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

;; (cadr (car (car elint-buffer-forms)))
;;    (if elint-top-form-logged

(when nil
  (defadvice elint-log-message (after acheck-elint-log-message
				      (errstr))
    "Get form name, location, and error message."
    (let* ((form (elint-top-form-form elint-top-form))
	   (top (car form))
	   (pos (elint-top-form-pos elint-top-form)))
      (message "form: %s %s %s" form top pos)))
  
  (ad-activate 'elint-log-message)
  
  (defun jmc-retest ()
    (elint-current-buffer)))

  



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

(defun acheck-make-overlay (lineno)
  (goto-char (point-min))
  (let (num (string-to-int lineno))
    (make-overlay (line-beginning-position num)
		  (line-end-position num))))

(defun acheck-parse (line)
  (when (x-pylint-parse line)
    (let ((ov (acheck-make-overlay (match-string 2 line))))
      (overlay-put ov 'acheck t)
      (acheck-pylint-annotate ov))))

(defun acheck-parsebuf (bufstr)
  (mapc acheck-parse (split-string bufstr "\n")))

(defun jmc-retest ()
  (find-file-other-window "sunfudge.py")
  (acheck-parse "sunfudge.py:1: [E, Fake] Undefined variable 'fudge'"))
;; (jmc-retest)
  
	     


; (modify-face 'acheck-master "foreground" nil)
;; (overlay-put (make-overlay (- (point-max) 50) (point-max))
;; 	     'face '(:underline "red2"))

 ;; (overlay-put (make-overlay (- (point-max) 50) (point-max))
 ;; 	     'face 'acheck-pylint)



