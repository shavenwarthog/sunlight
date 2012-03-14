;; John's Compile Helpers


;; XX buffer-local:
(defvar jmc-makefile-name "jm.mk" "Makefile for JMC project")
(set-variable 'jmc-makefile-name "dev.mk")


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
  (let ((compilation-directory (jmc-get-compilation-directory jmc-makefile-name))
	(compile-command 
	 (format
	  "make -f dev.mk TEST_FUNCTION=%s mytests"
	  (if (and (not current-prefix-arg) testfunc)
	      (format ":%s" testfunc)
	    "")
	  )))
    (recompile)))


(defun jmc-make-this-makefile ()
  (save-some-buffers t)
  (let ((compile-command 
	 (format
	  "make -f %s"
	  (buffer-name))))
    (recompile)))
  

(defun jmc-executablep (path)
  (and (not (null path))
       (> (logand (file-modes path) 73) 0)))


(defun jmc-runbuffer ()
  (interactive)
  (let ((compile-command 
	 (format "%s" (buffer-file-name))))
    (recompile)))


(defun jmc-retest ()
  "Depending on context, test current testfunction, current
testmodule, or current source module.

CONTEXT                 TEST
test_beer.py:testfunc	just testfunc
test_beer.py		whole test_beer.py module
beer.py			tests/test_beer.py
beer.py is +x		run ./beer.py
*compilation*		recompile
Makefile or *.mk	'make' this Makefile
"
  (interactive)
  (cond ((jmc-testmodulep)
	 (jmc-make-mytests (which-function)))
	((not (null (get-buffer "*compilation*")))
	 (recompile))
	((jmc-executablep (buffer-file-name))
	 (jmc-runbuffer))
	((eq major-mode 'makefile-gmake-mode)
	 (jmc-make-this-makefile))
	(t
	 (jmc-make-mytests ""))))


(defun jmc-insert-assertvalue ()
  (interactive)
  (let ((match (with-current-buffer "*compilation*"
		 (goto-char (point-min))
		 (if (re-search-forward "AssertionError: \\(.+\\) !=" nil t)
		     (match-string 1)))))
    (if match
	(insert match)
      (message "AsserionError not found in compilation buffer."))))


;; (add-hook 'find-file-hooks
;;  (lambda ()
;;    (when buffer-read-only
;;      (set-background-color "yellow"))))


(global-set-key (kbd "<f12>") 'jmc-insert-assertvalue)
(global-set-key (kbd "C-S-<return>") 'jmc-retest)
