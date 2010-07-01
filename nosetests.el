;; nosetests.el -- trim fat from post-test report

(require 'compile)
(defvar nosetests-hide t "Hide boring things")
;; (setq nosetests-hide t)

;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

(defun nosetests-current ()
  "Return name of unit test on this line or above.
Ex: 'ERROR: example.test_syntax' => 'test_syntax'
"
  (interactive)
  (save-excursion
    (when (bobp)
      (forward-line))
    (when (re-search-backward "^\\(ERROR\\|FAIL\\): .+\\.\\(.+\\)" nil nil)
      (match-string-no-properties 2))))
  

(defun nosetests-current-filepath ()
  (when (re-search-backward "^ *File \"\\(.+?\\)\"" nil nil)
    (match-string-no-properties 1)))


(defun nosetests-hide-match (matchnum)
  ""
  (put-text-property (match-beginning matchnum)
		     (match-end matchnum)
		     'invisible
		     'nosetests))


(defun nosetests-hide-region (pos1 pos2)
  ""
  (put-text-property pos1 pos2 'invisible 'nosetests))


(defun nosetests-jump-exception (&rest unused)
  "Set point to first line of exception, after the traceback."
  (interactive)
  (goto-char (point-max))
  (when (re-search-backward "^ *File" nil t)
    (re-search-forward "^[^ ]" nil t)))
  

(defun nosetests-hide-matching-lines (pat &rest numlines)
  "Scan through buffer, hiding lines in which the pattern matches."
  (let ((numlines (or numlines 1)))
    (goto-char (point-min))
    (while (re-search-forward pat nil t)
      (nosetests-hide-region
       (line-beginning-position)
       (+ numlines (line-end-position))))))

(defun nosetests-mapc-matches (pat func &optional buffer)
  (save-excursion
    (with-current-buffer (or buffer (current-buffer))
      (goto-char (point-max))
      (while (re-search-backward pat nil t)
      	(funcall func)))))

(defun nosetests-delete-line ()
  (delete-region (line-beginning-position) (line-beginning-position 2)))


;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

(defun nosetests-h-boring ()
  (interactive)
  (nosetests-mapc-matches
   "\\(.+/home.+?django/.+\\)"
   ))
;; (global-set-key (kbd "<kp-up>") 'nosetests-h-boring)
   

;; ;; TODO: disable next-error and hide instead of fully deleting
;; (defun nosetests-disable-boring ()    
;;   (nosetests-mapc-matches 
;;    (current-buffer) 
;;    "File .+ apply_clear_and_verify"
;;    'nosetests-delete-line)
;;   (nosetests-mapc-matches 
;;    (current-buffer) 
;;    "File .+ /eggs/"
;;    'nosetests-delete-line))

;; (global-set-key 
;;  [kp-multiply]
;;  '(lambda ()
;;     (interactive)
;;     (nosetests-disable-boring)))



;; X: port to mapc-matches
(defun nosetests-hide-all-matches (buffer pat matchnum)
  (save-excursion
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (re-search-forward pat nil t)
      	(nosetests-hide-match matchnum)))))

(defun nosetests-hide-path-prefixes (buffer)
  (nosetests-hide-all-matches buffer "File .\\(/home/.+?/src/.+?\\)" 1)
  (nosetests-hide-all-matches buffer "File .\\(.+?/\\)egg/" 1)
  (nosetests-hide-all-matches buffer "File .+\\(, line [0-9]+\\)" 1))

(defun nosetests-hide-boring (buffer)
  )
;; (defun nosetests-hide-paths (buffer)
;;   (nosetests-hide-all-matches 
;;    buffer 
;;    (concat 
;;     " *File .+eggs/\\("
;;     "fudge\\|nose"
;;     "\\).+\n")
;;    0))


(defun string-starts (long short)
  (string= (substring long 0 (length short)) short))
;; (string-starts "beer" "b") => t
;; (string-starts "beer" "x") => nil

(defun nosetests-path-boringp (path)
  (or (string-starts path "build/")
      (string-match "lib/python" path)
      ; (string-match "/eggs/" path
      ))
;; (nosetests-path-boringp "build/bdist.linux") => t
;; (nosetests-path-boringp "zoot.py") => nil
;; (nosetests-path-boringp "~/eggs/nose-0/case.py") => 1

;; (defun nosetests-hide-boring ()
;;   (goto-char (point-max))
;;   (while (nosetests-current-filepath)
;;     (nosetests-hide-region
;;      (line-beginning-position)
;;      (line-beginning-position 2))))

;; (defun nosetests-delete-boring ()
;;   (goto-char (point-max))
;;   (while (nosetests-current-filepath)
    
(defvar nosetests-hide-hooks nil "woo")
(add-hook 'nosetests-hide-hooks 'nosetests-hide-boring)

;; (defun NEW-nosetests-hide-decorations (&optional buffer status)
;;   (interactive)
;;   (let ((buffer (or buffer (current-buffer))))
;;     ;; (nosetests-hide-projpaths buffer)
;;     (save-excursion
;;       (with-current-buffer buffer
;; 	(run-hooks 'nosetests-hide-hooks)))))

;; run_tests harness:
(defun nosetests-custom ()
  (interactive)
  (mapc 'nosetests-hide-matching-lines 
	(list 
	 "^\\(Creat\\|Install\\)"
	 "^\\+ \\([A-Z][A-Z]\\).+$"
	 "^\\+\\+ \\([A-Z][A-Z]\\).+$"
	 )))

;;  (global-set-key [kp-delete] 'nosetests-custom)



(defun nosetests-hide-decorations (&optional buffer status)
  "Hide boilerplate, leaving print output, traceback, and exception details."
  (interactive)
  (when nosetests-hide
    (let ((buffer (or buffer (current-buffer))))
      (with-current-buffer buffer
	;; header:
	(goto-char (point-min))
	(when (re-search-forward "^=====.+$" nil t)
	  (nosetests-hide-region (point-min) (line-beginning-position)))))))
      
;; (defun nosetests-hide-decorations (&optional buffer status)
;;   "Hide boilerplate, leaving print output, traceback, and exception details.
;; "
;;   (interactive)
;;   (when nosetests-hide
;;     (let ((buffer (or buffer (current-buffer))))
;;       ;; (nosetests-hide-projpaths buffer)
;;       (save-excursion
;; 	(with-current-buffer buffer
;; 	  (nosetests-custom)
;; 	  ;; header:
;; 	  (when nil
;; 	    (goto-char (point-min))
;; 	    (when (re-search-forward " \\.\\.\\. " nil t)
;; 	      (nosetests-hide-region (point-min) (line-beginning-position))))
;; 	  (when nil
;; 	    (goto-char (point-min))
;; 	    (when (re-search-forward "^\\(ERROR\\|FAIL\\):" nil t)
;; 	      (nosetests-hide-region (point-min) (line-beginning-position))))
;; 	  ;; footer:
;; 	  (nosetests-hide-all-matching-lines "^Ran ")
;; 	  (nosetests-hide-all-matching-lines "^FAILED ")
;; 	  (nosetests-hide-all-matching-lines " exited ")
;; 	  (nosetests-hide-all-matching-lines "^Compilation finished ")
;; 	  (nosetests-hide-all-matching-lines " \\.\\.\\. ")
;; 	  ;; traceback:
;; 	  (nosetests-hide-path-prefixes buffer)	;; boring parts of file paths
;; 	  (nosetests-hide-paths buffer)	;; entire paths (keep code)
;; 	  ;; nonverbose: row of dots/Error/Fatal:
;; 	  (when (re-search-forward "^[.EF]+$" nil t)
;; 	    (nosetests-hide-region (point-min) (line-beginning-position)))
;; 	  ;; X deletions:
;; 	  (nosetests-disable-boring)
;; 	  )))))
(when t
  (global-set-key [kp-home] 'nosetests-hide-decorations)
  (global-set-key [C-kp-home] 'nosetests-unhide))




;; (when nil
;;   (global-set-key 
;;    [kp-home] 
;;    '(lambda () (interactive)
;;       (nosetests-hide-decorations (current-buffer) nil))))

(define-minor-mode nosetests-mode
  "Highlight useful Nosetest information."
  :lighter " JM"
  :keymap
  '(;;("\C-\^?" . hungry-electric-delete)
    ([kp-delete] . nosetests-toggle-hide)))

(setq compilation-finish-functions 
      '(nosetests-hide-decorations
	nosetests-maybe
	nosetests-jump-exception))

;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

(defun test ()
  (interactive)
  (compilation-start "nosetests -v example" 'nosetests-mode))
(defun eval-and-test ()
  (interactive)
  (forward-paragraph)
  (eval-last-sexp nil)
  (test))
;; (local-set-key [kp-enter] '(lambda () (interactive) (eval-last-sexp) (test)))
;; (local-set-key [kp-enter] 'test)


;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;; (defun nosetests-note-current ()
;;   (interactive)
;;   (let ((current (nosetests-current)))
;;     (when current
;;       (force-mode-line-update)
      
(defun nosetests-unhide ()
  "Make all stuff visible and buffer editable."
  (toggle-read-only -1)
  (interactive)
  (remove-text-properties (point-min)
                          (point-max)
			  '(invisible nil)))

(define-compilation-mode nosetests-mode "Zoot"
  "Shrink 'nosetests' output to focus on interesting bits."
  )

(defun nosetests ()
  (unless nil ;; XXX nosetests-mode
    (nosetests-hide-disable)))

(defvar nosetests-mode nil "")		;; XXXX
  
;; (define-minor-mode nosetests-mode
;;   "XX"
;;   nil " U" nil




(provide 'nosetests)


;; :::::::::::::::::::::::::::::::::::::::::::::::::: HISTORICAL

;; Unicode BOX DRAWINGS LIGHT HORIZONTAL = 2500
(defun nosetests-replace-line ()
  (goto-char (point-min))
  (while (re-search-forward "^---+" nil t)
    (replace-match (make-string 60 ?─)))
  (goto-char (point-min))
  (while (re-search-forward "^===+" nil t)
    (replace-match (make-string 60 ?═))))

(defvar nosetests-this 'nosetests-note-current
  "A function to call when `nosetests' is active.
The variable buffer will be dynamically bound to the current buffer
where activity is occuring.

Example value:

    (lambda (&rest ignore)
      (call-process \"play\" nil nil nil \"/some/file.ogg\"))")

(defun nosetests-maybe (&rest ignore)
  "When minor-mode `nosetests' is active, `nosetests-this' is called.
See `rcirc-activity-hooks' for more."
  (when nosetests-mode
    (run-with-idle-timer
     3 nil nosetests-this)))

