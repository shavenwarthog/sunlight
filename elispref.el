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

;; XXX: gauche but works

(defun elispref-render (funcname html)
  (html-mode)
  (princ lispref-html)
  (elispref-replace "&mdash;" "-")	;; XX
  (sgml-tags-invisible t))

(defun elispref (funcname)
  (interactive "aFunction: ")
  (save-excursion
    (set-buffer (find-file-noselect "~/Documents/elisp.html" nil t))
    (setq buffer-read-only t)		;;XX?
    (goto-char (point-min))
    (re-search-forward (concat "<b>" funcname "</b>"))
    (let* ((start (re-search-backward "<div"))
	   (end (re-search-forward "</div>"))
	   (lispref-html (buffer-substring-no-properties start end)))
    (save-excursion
      (with-help-window (help-buffer)
	(set-buffer (help-buffer))
      	(elispref-render funcname lispref-html))))))


(defun jmc-test () (elispref "mapcar"))


(global-set-key (kbd "C-h F") 'elispref)

