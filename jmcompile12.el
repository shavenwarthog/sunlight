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
