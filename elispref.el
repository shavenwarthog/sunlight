;; (defcustom elispref-css nil
;;   "CSS for Emacs Lisp Reference manual")

;; ;; code fixed
;; (setq elispref-css "
;; <style type=\"text/css\">
;; var { font-size:large; }
;; blockquote	{ margin: 0px; border: 0px; }
;; </style>
;; ")

;; (defun elispref-modify-html ()
;;   (goto-char (point-min))
;;   (insert elispref-css))

(defun elispref-replace (regexp to-string)
  (goto-char (point-min))
  (while (re-search-forward regexp (point-max) t)
    (replace-match to-string)))

(defun elispref (funcname)
  (interactive "aFunction: ")
  (find-file-other-window "~/Documents/elisp.html")
  (goto-char (point-min))
  (re-search-forward (format "<b>" funcname "</b>"))
  (let* ((start (re-search-backward "<div"))
	 (end (re-search-forward "</div>"))
	 (lispref-buf (current-buffer)))
    (switch-to-buffer (get-buffer-create (format "*elisp: %s*" funcname)))
    (erase-buffer)
    (insert-buffer-substring lispref-buf start end)
    (elispref-replace "&mdash;" "-")
    (html-mode)
    (sgml-tags-invisible t)))

;;    (w3-region (point-min) (point-max))))

(defun jmc-test () (elispref "1+"))



(when nil
  (save-excursion
    (find-file-other-window "~/Documents/elisp.html")
    (goto-char (point-min))
    (re-search-forward "<b>mapcar</b>")
    (let* ((start (re-search-backward "<div"))
	   (end (re-search-forward "</div>"))
	   (lispref-buf (current-buffer)))
      (save-current-buffer
	(set-buffer (get-buffer-create "*beer*"))
	(toggle-read-only -1)
	(erase-buffer)
	(insert-buffer-substring lispref-buf start end)
	(when nil
	  (goto-char (point-min))
	  (htmlr-render)
	  (goto-char (point-min))
	  (while (re-search-forward "&[a-z]+;" nil t)
	    (replace-match "X")))
	(view-buffer "*beer*")))))


;  (other-window 1))
    ;; (with-output-to-temp-buffer "beer" 
    ;;   (insert-buffer-substring lispref-buf start (re-search-forward "</div>"))))

   ;; (let ((oldbuf (current-buffer)))
   ;;       (save-current-buffer
   ;;         (set-buffer (get-buffer-create buffer))
   ;;         (insert-buffer-substring oldbuf start end))))

;; (defun jmc-testme ()
;;   (interactive)
;;   (eval-defun nil)
;;   (
(define-key emacs-lisp-mode-map [kp-enter]
  (lambda () (interactive) (eval-defun nil) (elispref)))
;; (global-set-key (kbd "<kp-enter>") 'elispref)
(global-set-key (kbd "C-h F") 'elispref)

