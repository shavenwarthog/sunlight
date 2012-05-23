;; loghelper4.el -- work with font-lock

(require 'hi-lock)

(defface loghelper-dim 
  '((t :foreground "gray20" :background nil)) "dark gray")

(defface loghelper-invisible 
  '((t :foreground "gray20" :background nil)) "invisible text (dark gray)")

(defface loghelper-yellow 
  '((t :foreground "yellow"  :weight bold)) "yellow face")

;; pattern format from font-lock.el:
;;	(SUBEXP FACENAME [OVERRIDE [LAXMATCH]])
;; We generally do (0 FACENAME t), to override colors for the entire regex
;;
(defun loghelper-hook ()
  (interactive)
  (hi-lock-set-file-patterns 
   '(
     ;; never-used source files in tracebacks
     (".*/lib/python.*" (0 'loghelper-invisible t))

     ;; dull compilation labels
     (".*\\(Entering\\|Leaving\\) directory.*" (0 'loghelper-dim t))
     ("^\\(real\\|user\\|sys\\).*" (0 'loghelper-dim t))

     ;; dull startup lines
     (".*Log opened.*" (0 '(face 'loghelper-invisible :invisible t) t))
     (".*new pool.*" (0 'loghelper-invisible t))
     (".*reactor class:.*" (0 'loghelper-invisible t))
     (".*starting on.*" (0 'loghelper-invisible t))
     (".*stale pidfile.*" (0 'loghelper-invisible t))
     (".*Starting factory.*" (0 'loghelper-invisible t))
     (".*twistd 10.0.*" (0 'loghelper-invisible t))
     )))

(add-hook 'compilation-mode-hook 'loghelper-hook)
