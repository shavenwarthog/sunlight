;; trimfat.el -- zap dull text from post-test report

(require 'compile)

(defcustom trimfat-flush-lines-regexp-list
  (list
   "File .+lib/python.+\n"
   "File .+/deps/.+\n")
  "Delete lines that match one of the regular expressions"
  :group 'trimfat)


(dolist (pat (list "^+" "^\\(Installing\\|Creat\\|Failed\\|Destroying\\)"
		   "^/.+/psql"
		   "File .+/packages/restapi/.+\n"
		   "File .+/tools/decorators.+\n"))		   
  (add-to-list 'trimfat-flush-lines-regexp-list pat))
 
	     
(defcustom trimfat-highlight-matches t 
  "Hide boring things, highlight goodies" 
  :group 'trimfat)
;; (setq trimfat-hide nil)


;; :::::::::::::::::::::::::::::::::::::::::::::::::: FACES

(defface trimfat-invisible
  '((t :foreground "invisible" :underline "green"))
  "Invisible face"
  :group 'trimfat)

(defface trimfat-okay
  '((((min-colors 88) (background dark))
     (:foreground "green2"))
    (((background dark)) (:background "green" :foreground "black"))
    (((min-colors 88)) (:background "green1"))
    (t (:background "green")))
  "Face"
  :group 'trimfat)

(defface trimfat-error
  '((((min-colors 88) (background dark))
     (:background "firebrick2"))
    (((background dark)) (:background "green" :foreground "black"))
    (((min-colors 88)) (:background "green1"))
    (t (:background "green")))
  "Face"
  :group 'trimfat)

;; (set-face-attribute 'trimfat-error nil :background "firebrick3")

;; trimfat-highlight

;; :::::::::::::::::::::::::::::::::::::::::::::::::: HELPERS

(defun trimfat-current ()
  "Return name of unit test on this line or above.
Ex: 'ERROR: example.test_syntax' => 'test_syntax'
"
  (interactive)
  (save-excursion
    (when (bobp)
      (forward-line))
    (when (re-search-backward "^\\(ERROR\\|FAIL\\): .+\\.\\(.+\\)" nil nil)
      (match-string-no-properties 2))))

;; :::::::::::::::::::::::::::::::::::::::::::::::::: 

(defun trimfat-hl-pats (face patlist)
  (interactive)
  (dolist (pat patlist)
    (hi-lock-face-buffer pat face)))


(defun trimfat-highlight-setup ()
  (trimfat-hl-pats 'trimfat-okay (list "!="))
  (trimfat-hl-pats 'trimfat-okay (list "\\(?: ... \\)ok$" "^OK$"))
  (trimfat-hl-pats 'trimfat-error (list " ... \\(ERROR\\|FAIL\\)$")))
  
(defun trimfat-replace (regexp to-string)
  (goto-char (point-min))
  (while (re-search-forward regexp (point-max) t)
    (replace-match to-string)))

;; (defun trimfat-test ()
;;   (toggle-read-only -1)			;; XX gauche
;;   (save-excursion
;;     (trimfat-replace "/home.+src/[^/]+/" ""))) ;; XX
;; ;; (add-hook 'compilation-filter-hook 'trimfat-test)


;; XX:
(defun trimfat-remove-overlays ()
  (remove-from-invisibility-spec '(trimfat . t))
  (remove-from-invisibility-spec 'trimfat))

(defun trimfat-f-flush-lines ()
  (save-excursion
    (dolist (pat trimfat-flush-lines-regexp-list)
      (flush-lines pat (point-min) (point-max)))))

(defun trimfat-f-fixup-invisible ()
  (save-excursion
    (while (not (eobp))			;; ??
      (let ((plist (text-properties-at (point)))
	    (next-change
	     (or (next-property-change (point) (current-buffer))
		 (point-max))))
	(if (string= "invisible" (get-text-property (point) :foreground))
	    (put-text-property (point) next-change 'invisible 'trimfat))
	(goto-char next-change)))))



(add-hook 'compilation-filter-hook 'trimfat-f-flush-lines)
(add-hook 'compilation-filter-hook 'trimfat-f-fixup-invisible)

;; :::::::::::::::::::::::::::::::::::::::::::::::::: MINOR MODE

(defun trimfat-exit-message-function (status code msg)
  (cons msg 
	(if (= code 0)
	    "ok"
	  (format "%d errs" 12))))

(defun trimfat-mode-hook ()
  ;; (when (eq trimfat-highlight-matches t)
  (set (make-local-variable 'compilation-exit-message-function)
       'trimfat-exit-message-function)
  (if t
      (progn
	(add-to-invisibility-spec '(trimfat . t)) ;; ellipses
	(trimfat-highlight-setup))
    (progn
      (remove-from-invisibility-spec '(trimfat . t))
      (remove-from-invisibility-spec 'trimfat))))

(add-hook 'compilation-mode-hook 'trimfat-mode-hook)

(provide 'trimfat)

;; :::::::::::::::::::::::::::::::::::::::::::::::::: HISTORICAL

(trimfat-hl-pats 'trimfat-invisible (list "beer"))
