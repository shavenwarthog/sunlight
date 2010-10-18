

(require 'winring)
(winring-initialize)

;; show: show entire ring
;; next: loop through window configurations
;; save: save current config as new
;; reset: zap all configurations

(global-set-key (kbd "<kp-subtract>") 'winring+-next)
(global-set-key (kbd "C-<kp-subtract>") 'winring+-save)
;; (global-set-key (kbd "<kp-enter>") 'winring+-show)
;; (global-set-key (kbd "C-<kp-multiply>") 'winring+-reset)
;; (global-set-key (kbd beer) 'winring-delete-configuration)

(defun winring+-next ()
  (interactive)
  (winring-next-configuration)
  (message "%s: current -- %s" winring-name (winring-get-ring)))

(defun winring+-save ()
  (interactive)
  (winring-set-name (funcall winring-name-generator))
  (winring-save-current-configuration)
  (message "%s: saved" (winring-name-of-current)))

(defun winring+-reset ()
  (interactive)
  (let ((frame (selected-frame))
	(ring (make-ring winring-ring-size)))
    (setq winring-name-index 1)
    (winring-set-frame-ring frame ring)
    ring))

(defun winring+-show ()
  (interactive)
  (message "ring: %s" (winring-get-ring)))

(provide 'winring+)