;; allowing relevant bits be more readable
;; see also: (list-faces-display)
;;
(require 'hi-lock)

;; consider:
;; (add-to-list 'compilation-error-regexp-alist '("File .\\(.*?\\)., line \\([0-9]+\\)" 1 2))
;; [Sat Jun 12 01:06:58 2010] [error] 0000056 pid=6795 /home/johnm/src/zoot/openpub/resource.py:74 DEBUG => []

;; XXXXX
(when nil
  (add-to-list 'compilation-error-regexp-alist '("\\([^ ]+.py\\):\\([0-9]+\\)" 1 2)))

(global-hi-lock-mode 1)

(defface hi-dim '((t :foreground "gray20" :background nil)) "dark gray")

(defface hi-invisible 
  '((t :foreground "green4" 
       :invisible t 
       ;; :weight 'ultra-light
       )) "documentation here")
(defface hi-yellow 
  '((t :foreground "yellow"  :weight bold)) "yellow face")

(defvar custcompile-hi-lock nil "zoot")
(setq custcompile-hi-lock t)

;;;        ("x.expects.+?," (0 'compilation-info t)) ; bright green
;;;        ("name='.*?'" (0 'button t)) ; underlined
;;;        ("key='.*?'" (0 'compilation-error t)) ; pinkish
;;;        ("was unexpected; Expected:"	(0 'hi-pink t)) ; pink background


(defun custcompile-hook ()
  (interactive)
  (when custcompile-hi-lock
    (hi-lock-set-file-patterns 
     '(
       ;; green project-specific highlights:
       ("twitter" (0 'compilation-info t)) ; bright green

       ;; yellow boxy generic markers:
       ("} != {" (0 'hi-yellow t))
       ("\) != \(" (0 'hi-yellow t))
       ("\\] != \\[" (0 'hi-yellow t))
       ))))
;;;         ("MBL\\S+" (0 'compilation-info t)) ; bright green
;;;         ("Mobile Site [A-z]+" (0 'compilation-info t)) ; bright green
;;;         ("mobile[^,]+" (0 'compilation-info t))
;;;         ("DESC\\S+" (0 'hi-yellow t))
;; (defun cassandra-be-good-hook () (interactive))
(add-hook 'compilation-mode-hook 'custcompile-hook)
