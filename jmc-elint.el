;; :::::::::::::::::::::::::::::::::::::::::::::::::: EMACS LISP LINT
(defun problem ()
  (let (x 5)
    123))

(defface elint-face
  '((t :foreground "gray80" :underline "gray"))
  "Emacs Lisp lint message available")

(defadvice elint-log-message (after elint-annotate (&optional errstr))
  (let* ((formpos (cdr elint-top-form))
	 (ov (make-overlay formpos (+ formpos 20))))
    (overlay-put ov 'elint t)
    (overlay-put ov 'face 'elint-face))
  (message "beer: %s / %s" errstr formpos))

(ad-activate 'elint-log-message)
(ad-update 'elint-log-message)

