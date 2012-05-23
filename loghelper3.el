;; loghelper.el -- allow relevant bits be more readable

(require 'hi-lock)

(defface loghelper-dim '((t :foreground "gray20" :background nil)) "dark gray")

(defface loghelper-invisible 
  '((t :foreground "green4" :invisible t)) "invisible text")

(defface loghelper-yellow 
  '((t :foreground "yellow"  :weight bold)) "yellow face")

(defvar loghelper-patterns 
  "Patterns")

(defvar loghelper-hi-lock nil "Use Loghelper colors or not")





;; (defun loghelper-compile-hook ()
;;   (interactive)
;;   (when loghelper-hi-lock


;; (defun loghelper-compile-hook ()
;;   (interactive)
;;   (when loghelper-hi-lock
;;     (hi-lock-set-file-patterns 
;;      '(
;;        ;; dim boring paths
;;        (".*/[Ee]nv/[Ll]ocal/[Ll]ib.*" (0 'loghelper-dim t))
;;        (".*/usr/lib/python.*" (0 'loghelper-dim t))

;;        ;; zap startup stuff
;;        (".*starting on.*" (0 'loghelper-dim t))
;;        (".*reactor class.*" (0 'loghelper-dim t))
;;        (".*Starting factory.*" (0 'loghelper-dim t))
;;        (".*Log opened.*" (0 'loghelper-dim t))
;;        (".*twistd .*" (0 'loghelper-dim t))
;;        (".*new pool.*" (0 'loghelper-dim t))
;;        (".*stale pidfile.*" (0 'loghelper-invisible t))
;;        ))))

(defun loghelper-apply-pattern (pat)
  (let* ((regex (car pat))
	 (pat-face (nth 1 pat)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex (point-max) t)
	(let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
	  (overlay-put overlay 'loghelper-overlay t)
	  (overlay-put overlay 'face pat-face)
	  (if (eq pat-face 'loghelper-invisible)
	      (overlay-put overlay 'invisible 'loghelper))
	  (goto-char (match-end 0)))))))

(defun loghelper-unapply ()
  (interactive)
  (remove-overlays nil nil 'loghelper-overlay t))

(defun loghelper-apply ()
  (interactive)
  (mapcar 'loghelper-apply-pattern loghelper-patterns))
  
(defun loghelper-init ()
  (interactive)
  (global-hi-lock-mode 1)
  (setq loghelper-hi-lock t)
  (add-hook 'compilation-mode-hook 'loghelper-compile-hook)
  (add-to-invisibility-spec '(loghelper . t))) ;; to display an ellipsis

(loghelper-init)
;; (loghelper-apply-pattern '("starting" loghelper-dim))
;; (loghelper-apply)
;; (loghelper-unapply)

(provide 'loghelper)
