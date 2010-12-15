;; compile, compile+run
;; another compile kills the first
;; log to *compilation* with cool colors

(load "actionscript-mode-haas-7.0")

(defun action-kill ()
  (interactive)
  (condition-case nil
      (kill-compilation)
    (error nil)))

(defun action-make-recompile ()
  (interactive)
  (action-kill)
  (jmc-make-recompile))
  ;; (run-at-time "5 sec" nil 'action-kill))

(global-set-key (kbd "<kp-enter>") 'action-make-recompile)
(global-set-key (kbd "<kp-add>") 'action-kill)


(add-to-list 'auto-mode-alist '("\\.as\\'" . actionscript-mode))







