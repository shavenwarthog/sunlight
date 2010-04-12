(require 'compile)

;; coverage:
;;	nosetests -sv --with-coverage --cover-package=mod1.mod2.zoot test_zoot.py
;;      ==>
;; Name         Stmts   Exec  Cover   Missing
;; mod1.mod2.zoot  73     42    57%   25, 37, 40, 43, 58, 116, 155-168


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
  

(defun nosetests-filepath ()
  (interactive)
  (when (re-search-backward "^ *File \"\\(.+?\\)\"" nil nil)
    (message (match-string-no-properties 1))))
;; (global-set-key [kp-add] 'nosetests-filepath)

(defun nosetests-hide-match (matchnum)
  ""
  (put-text-property (match-beginning matchnum)
		     (match-end matchnum)
		     'invisible
		     'nosetests))

(defun nosetests-hide-region (pos1 pos2)
  ""
  (put-text-property pos1 pos2 'invisible 'nosetests))

(defun nosetests-jump-exception ()
  "Set point to first line of exception, after the traceback."
  ;; =first outdented line after last "File" 
  (interactive)
  (goto-char (point-max))
  (re-search-backward "File " nil t))
;;    "beer"))
;; (re-search-forward "^\\S-" nil t)))
;; (global-set-key [kp-minus] 'nosetests-jump-exception)

;; -*- mode: compilation; default-directory: "~/src/indigo.git/digisynd/indigo/test/" -*-
(defun nosetest-zapline (pat)
  (goto-char (point-min))
  (when (re-search-forward pat nil t)
    (nosetests-hide-region (line-beginning-position) (1+ (line-end-position)))))
;; (nosetest-zapline "mode: compilation")

;; Ran 123 tests in woo


    (when nil
      (nosetest-zapline "mode: compilation")
      (nosetest-zapline "Compilation started")
      (nosetest-zapline "bin/nosetests")
      (nosetest-zapline "===="))

(defun nosetests-hide-decorations (buffer status)
  (with-current-buffer buffer
    ;; header:
    (goto-char (point-min))
    (when (re-search-forward "^FAIL:" nil t)
      (nosetests-hide-region (point-min) (line-beginning-position)))
    ;; footer:
    (goto-char (point-max))
    (when (re-search-backward "^---" nil t)
      (nosetests-hide-region (point-max) (line-beginning-position)))

    (when nil
      (nosetest-zapline "^Ran")
      (nosetest-zapline "Compilation exited")
      (nosetest-zapline "FAILED"))
    ;; "nosetests -v" has "testname ... ok"
    (nosetest-zapline " \\.\\.\\. ")
    ;; nonverbose: row of dots/Error/Fatal:
    (when (re-search-forward "^[.EF]+$" nil t)
      (nosetests-hide-region (point-min) (line-beginning-position)))))
;; (defun nosetests-hide-decorations (buffer status))


(defun nosetests-jump-up (buffer status)
  (with-current-buffer buffer
    (forward-line -10)))

(setq compilation-finish-functions 
      '(nosetests-hide-decorations
	nosetests-maybe
	nosetests-jump-up))

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
      
(defun nosetests-hide-disable ()
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

;; (add-hook 'rcirc-activity-hooks 'nosetests-maybe)

(provide 'nosetests)
