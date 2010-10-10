;; http://www.emacswiki.org/emacs/GenericMode
;; file:///home/johnm/Documents/elisp.html#Search_002dbased-Fontification

(define-generic-mode 'jm-log-mode
  () 
  ;; keyword-list
  '("DEBUG" "INFOx" "ERROR" "CRITICAL")
  ;; font-lock-list
  '(
    ".+ INFO" ;; font-lock-keyword-face
    ("\\(--.*\\)" 1 'font-lock-comment-face)
    )
  ;; auto-mode-list
  '("\\.log\\'")
  ;; function-list
  (list (lambda () (setq comment-start "--")))
  "Major mode for very simple log highlighting.")

