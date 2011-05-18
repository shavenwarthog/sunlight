XXX: replaced by builtin (which-func)

;; defun=outer level class/dev

(when t
  (global-set-key 
   (kbd "<kp-next>") 
   (lambda () (interactive) (message "woo: %s" (python-end-of-defun))))
  (global-set-key 
   (kbd "<kp-right>") 
   (lambda () (interactive) (message "woo: %s" (python-beginning-of-defun)))))

;; (set (make-local-variable 'pycontext-str) "PC")
(defcustom pycontext-str "" "woo")
(defun pycontext-callback ()
  (setq pycontext-str (python-current-defun)))


    