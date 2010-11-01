;; suntags.el -- tags for multiple dimensions per item

;; 1: defun system(cmd):
;; 2:   os.system(cmd)
;; 3:
;; 4: defun testme():
;; 5:   system("hostname")

;; 'system' defined line 1, called on line 2 and 5

(defvar suntag-define-table (make-hash-table :test 'equal)
  "Hash of symbol definitions.")
  
(defun suntag-parse (line)
  (when (string-match "\\([0-9]+\\):.+ \\(.+\\)" line)
    (let* ((lineno (match-string 1 line))
	   (symname (match-string 2 line))
	   (oldlist (gethash symname suntag-define-hash)))
      (message "oldlist: %s" oldlist)
      (puthash symname (cons lineno oldlist) suntag-define-hash))))
;; (suntag-parse "99:defun gin")

(defun suntag-list ()
  (maphash #'(lambda (key value) (message "woo: %s %s" key value))
	   suntag-define-hash))
;; (defalias 'jmc-retest 'suntag-list)


(defun suntag-parsefile (path)
  (setq suntag-define-hash (make-hash-table :test 'equal))
  (mapc 'suntag-parse 
	(process-lines 
	 "egrep" "-n" "--only-matching" "defun\s+([[:alnum:]]+)" path)))

(defun suntag-find-tag (tagname)
  (interactive)
  (let ((tagval (gethash "system" suntag-define-hash)))
    (if tagval
	(goto-line (string-to-int tagval))
      (error "No tags containing %s" tagname))))
;; (suntag-find-tag "system")
      
;; (gethash "system" suntag-define-hash)

;;   (interactive (find-tag-interactive "Find tag: "))
  
  
;; (defun jmc-retest () (suntag-parsefile "suntags.el"))
(defun jmc-retest () 
  (let ((h (make-hash-table :test 'equal)))
    (puthash 'x (list "gin") h)
    (puthash 'x (append (gethash 'x h) (list "beer")) h)
    (message "woo: %s" (gethash 'x h))))