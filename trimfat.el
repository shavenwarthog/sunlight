;; trimfat.el -- zap dull text from post-test report

(require 'compile)

(defcustom trimfat-zap-lines-regexp-list
  (list
   "File .+lib/python.+\n"
   "File .+/deps/.+\n")
  "Delete lines that match one of the regular expressions"
  :group 'trimfat)


(dolist (pat (list "^+" "^\\(Installing\\|Creat\\|Failed\\|Destroying\\)"
		   "^/.+/psql"
		   "File .+/packages/restapi/.+\n"
		   "File .+/tools/decorators.+\n"))		   
  (add-to-list 'trimfat-zap-lines-regexp-list pat))
 
	     
(defcustom trimfat-highlight-matches t 
  "Hide boring things, highlight goodies" 
  :group 'trimfat)
;; (setq trimfat-hide nil)

;; :::::::::::::::::::::::::::::::::::::::::::::::::: FACES

(defface trimfat-okay
  '((((min-colors 88) (background dark))
     (:foreground "green2"))
    (((background dark)) (:background "green" :foreground "black"))
    (((min-colors 88)) (:background "green1"))
    (t (:background "green")))
  "Face"
  :group 'trimfat)

(defface trimfat-error
  '((((min-colors 88) (background dark))
     (:background "firebrick2"))
    (((background dark)) (:background "green" :foreground "black"))
    (((min-colors 88)) (:background "green1"))
    (t (:background "green")))
  "Face"
  :group 'trimfat)

(set-face-attribute 'trimfat-error nil :background "firebrick3")


;; :::::::::::::::::::::::::::::::::::::::::::::::::: HELPERS

(defun trimfat-current ()
  "Return name of unit test on this line or above.
Ex: 'ERROR: example.test_syntax' => 'test_syntax'
"
  (interactive)
  (save-excursion
    (when (bobp)
      (forward-line))
    (when (re-search-backward "^\\(ERROR\\|FAIL\\): .+\\.\\(.+\\)" nil nil)
      (match-string-no-properties 2))))

;; :::::::::::::::::::::::::::::::::::::::::::::::::: 

(defun trimfat-hl-pats (face patlist)
  (interactive)
  (dolist (pat patlist)
    (hi-lock-face-buffer pat face)))


(defun trimfat-highlight-setup ()
  (trimfat-hl-pats 'trimfat-okay (list "\\(?: ... \\)ok$" "^OK$"))
  (trimfat-hl-pats 'trimfat-error (list " ... \\(ERROR\\|FAIL\\)$")))
  
(defun trimfat-replace (regexp to-string)
  (goto-char (point-min))
  (while (re-search-forward regexp (point-max) t)
    (replace-match to-string)))

(defun trimfat-test ()
  (toggle-read-only -1)			;; XX gauche
  (save-excursion
    (trimfat-replace "/home.+src/[^/]+/" "")))
;; (add-hook 'compilation-filter-hook 'trimfat-test)


(defun trimfat-zap-lines ()
  (save-excursion
    (dolist (pat trimfat-zap-lines-regexp-list)
      (flush-lines pat (point-min) (point-max)))))



(add-hook 'compilation-filter-hook 'trimfat-zap-lines)

;; :::::::::::::::::::::::::::::::::::::::::::::::::: MINOR MODE

(defun trimfat-exit-message-function (status code msg)
  (cons msg 
	(if (= code 0)
	    "ok"
	  (format "%d errs" 12))))

(defun trimfat-mode-hook ()
  ;; (when (eq trimfat-highlight-matches t)
  (set (make-local-variable 'compilation-exit-message-function)
       'trimfat-exit-message-function)
  (trimfat-highlight-setup))

(add-hook 'compilation-mode-hook 'trimfat-mode-hook)

(provide 'trimfat)

















;; (global-set-key (kbd "<kp-up>") 'trimfat-highlight)


;; (defun trimfat-hide-match (&optional matchnum)
;;   ""
;;   (put-text-property (match-beginning (or matchnum 0))
;; 		     (match-end (or matchnum 0))
;; 		     'invisible
;; 		     'trimfat))

;; (defun trimfat-hide-region (pos1 pos2)
;;   (put-text-property pos1 pos2 'invisible 'trimfat))

;; (defun trimfat-mapc-matches (pat func &optional buffer)
;;   (save-excursion
;;     (with-current-buffer (or buffer (current-buffer))
;;       (goto-char (point-max))
;;       (while (re-search-backward pat nil t)
;;       	(funcall func)))))

;; (defun trimfat-hidepats (pats)
;;   (mapc (lambda (pat) (trimfat-mapc-matches pat 'trimfat-hide-match))
;; 	pats))

;; (defun trimfat-show-testfunc (&optional buffer)
;;   (interactive)
;;   (when trimfat-hide
;;     (let ((buffer (or buffer (current-buffer))))
;;       (with-current-buffer buffer
;; 	(trimfat-hidepats (list "test_"))))))
  
;; (defun trimfat-h-header (&optional buffer)
;;   "Hide boilerplate, leaving print output, traceback, and exception details."
;;   (interactive)
;;   (when trimfat-hide
;;     (let ((buffer (or buffer (current-buffer))))
;;       (with-current-buffer buffer
;; 	;; header:
;; 	(goto-char (point-min))
;; 	(when (re-search-forward "^=====.+$" nil t)
;; 	  (trimfat-hide-region (point-min) (line-beginning-position)))))))

;; (defun trimfat-h-footer (&optional buffer)
;;   "Hide boilerplate"
;;   (interactive)
;;   (trimfat-mapc-matches "^Ran \[0-9\]+ test.+\n" 'trimfat-hide-match)
;;   (trimfat-mapc-matches "^Compilation.+\n" 'trimfat-hide-match))
;; ;; (global-set-key (kbd "<kp-home>") 'trimfat-h-footer)

;; ;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;; ;; zap server/django-admin.py and server/tests.py

;; (defun trimfat-hide-fluff (&optional buffer status)
;;   "Hide boilerplate, leaving print output, traceback, and exception details."
;;   (interactive)
;;   (when trimfat-hide
   ;;  (trimfat-highlight)		;XX: run at beginning
;;     (trimfat-mapc-matches "/home/johnm/src/[^/]+/" 'trimfat-hide-match)
;;     (trimfat-mapc-matches 
;;      "\s+File .+/server/[^/]+.py.+\n.+\n" 
;;      'trimfat-hide-match)
;;     (trimfat-mapc-matches "\s+File .+in eq\n.+\n" 'trimfat-hide-match)
;;     (trimfat-mapc-matches "\s+File .+in eq_\.+\n.+\n" 'trimfat-hide-match)
;;     (trimfat-mapc-matches "\s+File .+/deps/.+\n.+\n" 'trimfat-hide-match)
;;     (trimfat-mapc-matches "\s+File .+/packages/.+\n.+\n" 'trimfat-hide-match)
;;     (trimfat-mapc-matches "\s+File .+/usr/lib/python.+\n.+\n" 'trimfat-hide-match)
;;     (trimfat-mapc-matches "/.+/bin/psql.+\n" 'trimfat-hide-match)
;;     (trimfat-h-header)
;;     (trimfat-h-footer)))

;; (defun trimfat-unhide ()
;;   "Make all stuff visible and buffer editable."
;;   (toggle-read-only -1)
;;   (interactive)
;;   (remove-text-properties (point-min)
;;                           (point-max)
;; 			  '(invisible nil)))

;; (define-key compilation-mode-map (kbd "<kp-home>") 'trimfat-unhide)

;; ;; ;; Unicode BOX DRAWINGS LIGHT HORIZONTAL = 2500
;; ;; (defun trimfat-replace-line ()
;; ;;   (goto-char (point-min))
;; ;;   (while (re-search-forward "^---+" nil t)
;; ;;     (replace-match (make-string 60 ?─)))
;; ;;   (goto-char (point-min))
;; ;;   (while (re-search-forward "^===+" nil t)
;; ;;     (replace-match (make-string 60 ?═))))
  
;; (add-to-list 'compilation-finish-functions 'trimfat-hide-fluff)
      
