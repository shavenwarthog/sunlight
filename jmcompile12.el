;; John's Compile Helpers


(defun jmc-get-compilation-directory (makefile)
  (let ((mkpath (locate-file makefile '("." ".."))))
    (when mkpath
      (file-name-directory mkpath))))


(defun jmc-testmodulep ()
  (string-match-p "^test" (buffer-name)))


(defun jmc-make-mytests (testfunc)
  "Run 'make mytests', testing this function or entire module.
With prefix arg, always test module.
"
  (interactive)
  (save-some-buffers t)
  (let ((compilation-directory (jmc-get-compilation-directory "dev.mk"))
	(compile-command 
	 (format
	  "make -f dev.mk TEST_FUNCTION=%s mytests"
	  (if (and (not current-prefix-arg) testfunc)
	      (format ":%s" testfunc)
	    "")
	  )))
    (recompile)))


(defun jmc-python-test-something ()
  "Depending on context, test current testfunction, current
testmodule, or current source module.

test_beer.py:testfunc	just testfunc
test_beer.py		whole test_beer.py module
beer.py			tests/test_beer.py
"
  (interactive)
  (if (jmc-testmodulep)
      (jmc-make-mytests (which-function))
    (jmc-make-mytests "")))




(defun jmc-insert-assertvalue ()
  (interactive)
  (let ((match (with-current-buffer "*compilation*"
		 (goto-char (point-min))
		 (if (re-search-forward "AssertionError: \\(.+\\) !=" nil t)
		     (match-string 1)))))
    (if match
	(insert match)
      (message "AsserionError not found in compilation buffer."))))


(global-set-key (kbd "<f12>") 'jmc-insert-assertvalue)
(global-set-key (kbd "C-S-<return>") 'jmc-python-test-something)
