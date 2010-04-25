;; C-? = dir(obj)
(defun ih-dir-object (symbol)
  (python-send-string 
   (format 
    "list( (name for name in dir(%s) if not name.startswith('_')) )"
    symbol)))

(defun ih-zoot ()
  (interactive)
  (ih-dir-object (thing-at-point 'symbol)))
  
;; (global-set-key (kbd "C-'") 'ih-zoot)