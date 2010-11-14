;; suntags.el -- tags for multiple dimensions per item

;; 3: def system(cmd):
;; 4:   os.system(cmd)
;; 5:
;; 6: def testme():
;; 7:   system("hostname")

;; 'system' defined line 3, called on line 7

(defvar suntag-define-table (make-hash-table :test 'equal)
  "Hash of symbol definitions.")
(defvar suntag-calledby-table (make-hash-table :test 'equal)
  "Hash of X calls Y, keyed on Y.")
  
(defun suntag-list1 (table)
  (maphash #'(lambda (key value) (princ (format "%s\t%s\n" key value))) table))

(defun suntag-list ()
  (with-output-to-temp-buffer "*Suntags List*"
    (princ "Definitions:\n")
    (suntag-list1 suntag-define-table)
    (princ "\nCalled by:\n")
    (suntag-list1 suntag-calledby-table)))

(defun jmc-test ()
  (clrhash suntag-define-table)
  (suntag-parse "suntags.el")
  (suntag-list))

(defun suntag-p-defs (table line)
  (when (string-match "\\([0-9]+\\):.+ \\(.+\\)" line)
    (let* ((lineno (match-string 1 line))
	   (symname (match-string 2 line))
	   (oldlist (gethash symname hash)))
      (puthash symname (cons lineno oldlist) hash))))

(defun suntag-parse-defs (path)
  (setq suntag-define-table (make-hash-table :test 'equal))
  (mapc (apply-partially 'suntag-p-defs suntag-define-table)
	(process-lines 
	 "egrep" "-n" "--only-matching" "def\s+([[:alnum:]]+)" path)))

(defun suntag-p-calledby (table line)
  (let ((words (split-string line)))
    (message "woo")
    (when (= 4 (length words))
      (let* ((lineno (cadr words))
	     (symname (nth 2 words))
	     (oldlist (gethash symname table)))
	(puthash symname (cons (cons lineno (nth 3 words)) oldlist) table)))))

(defun suntag-parse-calledby (path)
  (setq suntag-calledby-table (make-hash-table :test 'equal))
  (mapc (apply-partially 'suntag-p-calledby suntag-calledby-table)
	(process-lines "python2.6" "./calledby.py" path)))

(defun suntag-parse (path)
  (suntag-parse-calledby path)
  (suntag-parse-defs path))

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
