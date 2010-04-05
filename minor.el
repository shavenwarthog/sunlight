
(require 'compile)

(define-compilation-mode unittest-mode "Zoot"
  "howdy"
  )
(defun unittest-finished (buffer status)
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "\nFAIL:")
    (delete-region (point-min) (point))
    (goto-char (point-max))
    (re-search-backward "\n-*\nRan ")
    (delete-region (point-max) (point))))
  
(setq compilation-finish-functions '(unittest-finished))
(defun test ()
  (interactive)
  (compilation-start "nosetests -v example" 'unittest-mode))
;; (local-set-key [kp-enter] '(lambda () (interactive) (eval-last-sexp) (test)))
(local-set-key [kp-enter] 'test)