(require 'compile)

(define-compilation-mode unittest-mode "Zoot"
  "howdy"
  )


(defun unittest-current ()
  "Return name of unit test on this line or previous.
Ex: 'ERROR: example.test_syntax' => 'test_syntax'
"
  (interactive)
  (save-excursion
    (when (bobp)
      (forward-line))
    (when (re-search-backward "^\\(ERROR\\|FAIL\\): .+\\.\\(.+\\)" nil nil)
      (match-string-no-properties 2))))
;; (global-set-key [kp-add] 'unittest-current)
  

(defun unittest-filepath ()
  (interactive)
  (when (re-search-backward "^ *File \"\\(.+?\\)\"" nil nil)
    (message (match-string-no-properties 1))))
;; (global-set-key [kp-add] 'unittest-filepath)

(defun unittest-hide-match (matchnum)
  ""
  (put-text-property (match-beginning matchnum)
		     (match-end matchnum)
		     'invisible
		     'unittest))

(defun unittest-hide-region (pos1 pos2)
  ""
  (put-text-property pos1 pos2 'invisible 'unittest))

(defun unittest-hide-disable ()
  "Make all parens visible and buffer editable."
  ;; (toggle-read-only -1)
  (interactive)
  (remove-text-properties (point-min)
                          (point-max)
			  '(invisible nil)))

(defun unittest-finished (buffer status)
  (with-current-buffer buffer
    (goto-char (point-min))
    ;; "nosetests -v" has "testname ... ok"
    (when (re-search-forward " \\.\\.\\. " nil t)
      (unittest-hide-region (point-min) (line-beginning-position)))
    ;; nonverbose: row of dots/Error/Fatal:
    (when (re-search-forward "^[.EF]+$" nil t)
      (unittest-hide-region (point-min) (line-beginning-position)))
    ;; footer: line, then "Ran 23 tests in 0.026s":
    (goto-char (point-max))
    (when (re-search-backward "\n-+\nRan " nil t)
      (unittest-hide-region (point-max) (point)))))
  
(setq compilation-finish-functions 
      '(unittest-finished
	unittest-maybe))

;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

(defun test ()
  (interactive)
  (compilation-start "nosetests -v example" 'unittest-mode))
(defun eval-and-test ()
  (interactive)
  (forward-paragraph)
  (eval-last-sexp nil)
  (test))
;; (local-set-key [kp-enter] '(lambda () (interactive) (eval-last-sexp) (test)))
;; (local-set-key [kp-enter] 'test)


;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;; (defun unittest-note-current ()
;;   (interactive)
;;   (let ((current (unittest-current)))
;;     (when current
;;       (force-mode-line-update)
      

(define-minor-mode unittest-mode
  "XX"
  nil " U" nil
  (unless unittest-mode
    (unittest-hide-disable)))




(defvar unittest-this 'unittest-note-current
  "A function to call when `unittest' is active.
The variable buffer will be dynamically bound to the current buffer
where activity is occuring.

Example value:

    (lambda (&rest ignore)
      (call-process \"play\" nil nil nil \"/some/file.ogg\"))")

(defun unittest-maybe (&rest ignore)
  "When minor-mode `unittest' is active, `unittest-this' is called.
See `rcirc-activity-hooks' for more."
  (when unittest
    (run-with-idle-timer
     3 nil unittest-this)))

;; (add-hook 'rcirc-activity-hooks 'unittest-maybe)

(provide 'unittest)
