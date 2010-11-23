;; atest.el -- asynchronously mark tests as ok/fail/error

(defvar atest-state-alist nil ".")

(defface atest-error
  '((t :box "red2"))
  "error")
(defface atest-fail
  '((t :box "IndianRed"))
  "fail")
(defface atest-ok
  '((t :box "green3"))
  "ok")

(defun atest-remove-overlays ()
  (interactive)
  (remove-overlays nil nil 'atest t))

(defun atest-make-overlay (testname)
  (save-excursion
    (goto-line (point-min))
    (when (re-search-forward testname nil t)
      (let ((ov (make-overlay (line-beginning-position)
			      (line-end-position))))
	(overlay-put ov 'atest t)
	ov))))

(defun atest-filter (proc bufstr)
  (let ((start nil))
    (setq atest-state-alist nil)	;XXX
    (while (setq start 
		 (string-match "(.*\\.\\(.+\\)) ... \\(.+\\)"
			       bufstr start))
      (push (list (match-string 1 bufstr)
		  (match-string 2 bufstr))
	    atest-state-alist)
      (setq start (1+ start)))))

(defun jmc-test ()
  (atest-remove-overlays)
  (overlay-put (atest-make-overlay "WOO") 'face 'atest-fail))


(defun atest-annotate-test (testname status)
  (let ((facename (concat "atest-" (downcase "FAIL"))))
    (overlay-put (atest-make-overlay testname) 'face facename)))

(defun jmc-test() (atest-annotate-test "WOO" "ok"))

(defun atest-annotate ()
  (atest-remove-overlays)
  (dolist (elt atest-state-alist)
    (apply 'atest-annotate-test elt)))

;; def BeerTest():
;; def FoodTest():


(defun jmc-test () (atest-annotate)) 
;;;   (atest-filter 
;;;    nil
;;;    "beer (a.b.BeerTest) ... ok\n
;;; food (c.d.FoodTest) ... ERROR\n"))
 

(provide 'atest)


