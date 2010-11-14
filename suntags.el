;; suntags.el -- tags for multiple dimensions per item

;; 3: def system(cmd):
;; 4:   os.system(cmd)
;; 5:
;; 6: def testme():
;; 7:   system("hostname")

;; 'system' defined line 3, called on line 7

(defvar suntag-define-table (make-hash-table :test 'equal)
  "Hash of symbol definitions.")
  
(defun suntag-parse (hash line)
  (when (string-match "\\([0-9]+\\):.+ \\(.+\\)" line)
    (let* ((lineno (match-string 1 line))
	   (symname (match-string 2 line))
	   (oldlist (gethash symname hash)))
      (puthash symname (cons lineno oldlist) hash))))

(defun suntag-list ()
  (with-output-to-temp-buffer "*Suntags List*"
    (maphash #'(lambda (key value) (princ (format "%s\t%s\n" key value)))
	     suntag-define-table)))


(defun jmc-test ()
  (clrhash suntag-define-table)
  (suntag-parsefile "suntags.el")
  (suntag-list))

(defun suntag-parsefile (path)
  (setq suntag-define-table (make-hash-table :test 'equal))
  (mapc (apply-partially 'suntag-parse suntag-define-table)
	(process-lines 
	 "egrep" "-n" "--only-matching" "def\s+([[:alnum:]]+)" path)))

(defun suntag-find-tag (tagname)
  (interactive)
  (let ((tagval (gethash "system" suntag-define-table)))
    (if tagval
	(goto-line (string-to-int (car tagval)))
      (error "No tags containing %s" tagname))))
;; (suntag-find-tag "system")
      
;; (gethash "system" suntag-define-table)

;;   (interactive (find-tag-interactive "Find tag: "))
  
  
;; (defun jmc-retest () (suntag-parsefile "suntags.el"))
;; (defun jmc-retest () 
;;   (let ((h (make-table-table :test 'equal)))
;;     (puthash 'x (list "gin") h)
;;     (puthash 'x (append (gethash 'x h) (list "beer")) h)
;;     (message "woo: %s" (gethash 'x h))))
