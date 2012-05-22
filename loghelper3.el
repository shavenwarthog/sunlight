;; allow relevant bits be more readable

(require 'hi-lock)

(defface lh-dim '((t :foreground "gray20" :background nil)) "dark gray")

(defface lh-invisible 
  '((t :foreground "green4" :invisible t)) "invisible text")

(defface lh-yellow 
  '((t :foreground "yellow"  :weight bold)) "yellow face")

(defvar lh-patterns 
  "Patterns")

(defvar lh-hi-lock nil "Use Loghelper colors or not")

(defun lh-compile-hook ()
  (interactive)
  (when lh-hi-lock
    (hi-lock-set-file-patterns 
     '(
       ;; dim boring paths
       (".*/[Ee]nv/[Ll]ocal/[Ll]ib.*" (0 'lh-dim t))
       (".*/usr/lib/python.*" (0 'lh-dim t))

       ;; and startup stuff
       (".*starting on.*" (0 'lh-dim t))
       (".*reactor class.*" (0 'lh-dim t))
       (".*Starting factory.*" (0 'lh-dim t))
       (".*Log opened.*" (0 'lh-dim t))
       (".*twistd .*" (0 'lh-dim t))
       (".*new pool.*" (0 'lh-dim t))
       (".*stale pidfile.*" (0 'lh-invisible t))
       ))))



(defun lh-apply-pattern (pat)
  (let* ((regex (car pat))
	 (pat-face (nth 1 pat)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex (point-max) t)
	(let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
	  (overlay-put overlay 'loghelper-overlay t)
	  (overlay-put overlay 'face pat-face)
	  (if (eq pat-face 'lh-invisible)
	      (overlay-put overlay 'invisible 'loghelper))
	  (goto-char (match-end 0)))))))

;; (lh-apply-pattern '("zoot" lh-invisible))
;; (lh-unapply)

(setq lh-patterns
      '(("/[Ee]nv/[Ll]ocal/[Ll]ib.*" 'lh-invisible)
	(".*/usr/lib/python.*" 'lh-invisible)

       ;; and startup stuff
       (".*starting on.*" 'lh-invisible)
       (".*reactor class.*" 'lh-invisible)
       (".*Starting factory.*" 'lh-invisible)
       (".*Log opened.*" 'lh-invisible)
       (".*twistd .*" 'lh-invisible)
       (".*new pool.*" 'lh-invisible)
       (".*stale pidfile.*" 'lh-invisible)))

(defun lh-unapply ()
  (interactive)
  (remove-overlays nil nil 'loghelper-overlay t))

(defun lh-apply ()
  (interactive)
  (mapcar 'lh-apply-regex lh-patterns)) 
  
(defun lh-init ()
  (interactive)
  (global-hi-lock-mode 1)
  (setq lh-hi-lock t)
  (add-hook 'compilation-mode-hook 'lh-compile-hook)
  (add-to-invisibility-spec '(loghelper . t))) ;; to display an ellipsis



