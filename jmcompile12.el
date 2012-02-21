;; John's Compile Helpers

(defun jmc-python-test-function ()
  "Run 'make mytests', testing this function or entire module.
With prefix arg, always test module.
"
  (interactive)
  (save-some-buffers t)
  (let ((compile-command 
	 (format
	  "make -f dev.mk TEST_FUNCTION=%s mytests"
	  (if (and (not current-prefix-arg) (which-function))
	      (format ":%s" (which-function))
	    "")
	  )))
    (recompile)))

(global-set-key (kbd "C-S-<return>") 'jmc-python-test-function)


(defun jmc-insert-assertvalue ()
  (interactive)
  (let ((match (with-current-buffer "*compilation*"
		 (if (re-search-forward "AssertionError: \\(.+\\) !=" nil t)
		     (match-string 1)))))
    (if match
	(insert match)
      (message "AsserionError not found in compilation buffer."))))

(global-set-key (kbd "<f12>") 'jmc-insert-assertvalue)

;; (defun jmc-debug ()
;;   (interactive)
;;   (save-some-buffers t)
;;   (perldb "perl -d /home/johnm/work/kamta/z.t"))
;; (global-set-key (kbd "<f5>") 'jmc-debug)

;; ;; (defun jmc-testnow (&optional debug)
;; ;;   (interactive "p")
;; ;;   (if debug
;; ;;       (jmc-debug)
;; ;;     (recompile)))
;; (global-set-key (kbd "C-<return>") 'recompile)


;; (defun jmc-perl-set-breakpoint ()
;;   (interactive)
;;   (when t
;;     (beginning-of-line 1)		;; beginning of previous line
;;     (indent-according-to-mode)
;;     (insert "$DB::single = 2;\n")
;;     (indent-according-to-mode)))

;; (global-set-key (kbd "C-<f5>") 'jmc-perl-set-breakpoint)

;; (defun jmc-perl-dumper ()
;;   (interactive)
;;   (let* ((variable (thing-at-point 'sexp))
;; 	 (msg (format "print Dumper(%s), \" %s\\n\"; return;" variable variable)))
;;     (when t
;;       (beginning-of-line 1)		;; beginning of previous line
;;       (indent-according-to-mode)
;;       (insert msg)
;;       (insert "\n")
;;       (indent-according-to-mode))
;;     (message msg)))
;; (global-set-key (kbd "<f10>") 'jmc-perl-dumper)

;; ;; print Dumper(@status), " status\n"; return;



;; (defface jmc-todo
;;   '((t (:background "tomato")))
;;   "JMC face for TODO lines in code")
;; (defface jmc-test
;;   '((t (:background "yellow" :foreground "black")))
;;   "JMC face for Test stanza")
;; (defface jmc-syncpoint
;;   '((t (:foreground "yellow3" :weight 'ultra-bold)))
;;   "JMC face for synchronization points (AE condition variables)")

;; ;; (set-face-attribute 'jmc-test nil :background "yellow" :foreground "black")
;; ;; (set-face-attribute 'jmc-todo nil :background "tomato")

;; (defun jmc-highlight-test ()
;;   (interactive)
;;   (highlight-regexp ".*:.*Test.*" 'jmc-test)
;;   (highlight-regexp ".*TODO.*" 'jmc-todo)
;;   (highlight-regexp "cv.+\\(send\\|begin\\).+" 'jmc-syncpoint)
;;   (highlight-regexp ".+cv.+recv" 'jmc-syncpoint)
;;   (highlight-regexp "\\bcv\\b" 'jmc-syncpoint))

;; (global-set-key (quote [C-f12]) (quote jmc-highlight-test))


;; (defun jmc-perl-function-name ()
;;   (when (which-function)
;;     (car (last (split-string (car (which-function)) "::")))))

;; ;;  compilation-directory 
;; (defun jmc-perl-do-test-function ()
;;   (interactive)
;;   (let ((funcname (jmc-perl-function-name)))
;;     (compile (format "make TEST=%s test"
;; 		     funcname))))
  

;; (global-set-key [f12] 'jmc-perl-do-test-function)

