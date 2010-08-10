;; nosetests.el -- trim fat from post-test report

(require 'compile)
(defvar nosetests-hide t "Hide boring things")
;; (setq nosetests-hide nil)

;; :::::::::::::::::::::::::::::::::::::::::::::::::: HELPERS

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

(defun nosetests-hide-match (&optional matchnum)
  ""
  (put-text-property (match-beginning (or matchnum 0))
		     (match-end (or matchnum 0))
		     'invisible
		     'nosetests))

(defun nosetests-hide-region (pos1 pos2)
  (put-text-property pos1 pos2 'invisible 'nosetests))

(defun nosetests-mapc-matches (pat func &optional buffer)
  (save-excursion
    (with-current-buffer (or buffer (current-buffer))
      (goto-char (point-max))
      (while (re-search-backward pat nil t)
      	(funcall func)))))

(defun nosetests-hidepats (pats)
  (mapc (lambda (pat) (nosetests-mapc-matches pat 'nosetests-hide-match))
	pats))


(defun nosetests-h-header (&optional buffer)
  "Hide boilerplate, leaving print output, traceback, and exception details."
  (interactive)
  (when nosetests-hide
    (let ((buffer (or buffer (current-buffer))))
      (with-current-buffer buffer
	;; header:
	(goto-char (point-min))
	(when (re-search-forward "^=====.+$" nil t)
	  (nosetests-hide-region (point-min) (line-beginning-position)))))))

(defun nosetests-h-footer (&optional buffer)
  "Hide boilerplate"
  (interactive)
  (nosetests-mapc-matches "^Ran \[0-9\]+ test.+\n" 'nosetests-hide-match)
  (nosetests-mapc-matches "^Compilation.+\n" 'nosetests-hide-match))
;; (global-set-key (kbd "<kp-home>") 'nosetests-h-footer)

;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;; zap server/django-admin.py and server/tests.py

(defun nosetests-hide-fluff (&optional buffer status)
  "Hide boilerplate, leaving print output, traceback, and exception details."
  (interactive)
  (when nosetests-hide
    (nosetests-mapc-matches "/home/johnm/src/[^/]+/" 'nosetests-hide-match)
    (nosetests-mapc-matches 
     "\s+File .+/server/[^/]+.py.+\n.+\n" 
     'nosetests-hide-match)
    (nosetests-mapc-matches "\s+File .+/deps/.+\n.+\n" 'nosetests-hide-match)
    (nosetests-mapc-matches "\s+File .+/packages/.+\n.+\n" 'nosetests-hide-match)
    (nosetests-mapc-matches "\s+File .+/usr/lib/python.+\n.+\n" 'nosetests-hide-match)
    (nosetests-mapc-matches "/.+/bin/psql.+\n" 'nosetests-hide-match)
    (nosetests-h-header)
    (nosetests-h-footer)))

(defun nosetests-unhide ()
  "Make all stuff visible and buffer editable."
  (toggle-read-only -1)
  (interactive)
  (remove-text-properties (point-min)
                          (point-max)
			  '(invisible nil)))

(define-key compilation-mode-map (kbd "<kp-home>") 'nosetests-unhide)

;; ;; Unicode BOX DRAWINGS LIGHT HORIZONTAL = 2500
;; (defun nosetests-replace-line ()
;;   (goto-char (point-min))
;;   (while (re-search-forward "^---+" nil t)
;;     (replace-match (make-string 60 ?─)))
;;   (goto-char (point-min))
;;   (while (re-search-forward "^===+" nil t)
;;     (replace-match (make-string 60 ?═))))
  
(add-to-list 'compilation-finish-functions 'nosetests-hide-fluff)
      
(provide 'nosetests)
