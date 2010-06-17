
;; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: SETUP

; in order of most common to least:
; ENTER: test quickly, current test func, or fast tests in this file
; ADD: test fully: all tests in this file
; SUBTRACT: toggle direction
;
; "Direction" means XXX
;

;; ; GLOBALS:
;; ;
;; (global-set-key [kp-insert] 'jmc-next)
;; (global-set-key [kp-subtract] 'jmc-toggle-direction)
;; (global-set-key [C-return] 'jmc-eval-and-test) ; elisp only

;; ; common, but overridden:
;; (global-set-key [kp-enter] 'jmc-make-fast)
;; (global-set-key [kp-add] 'jmc-make-test)
;; )
;; ; EMACS LISP:
;; ;
;; (when nil
;;   (defun jmc-emacs-lisp-hook ()
;;     (define-key emacs-lisp-mode-map [kp-enter] 'elint-current-buffer)
;;     (define-key emacs-lisp-mode-map [kp-add] 'next-error))
;;   (add-hook 'emacs-lisp-mode-hook 'jmc-emacs-lisp-hook))

;;   ;; (local-set-key [kp-enter] 'elint-current-buffer)


;; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: UTILS


(defun jmc-has-word-p (word)
  "True if WORD found in next few lines."
  (not (eq nil (word-search-forward word 1000 t))))


(defun jmc-py-has-tests-p ()
  "Return true if current buffer contains unit tests.
IE: search for 'nose' or 'unittest' imports."
  (save-excursion
    (goto-char 0)
    (or (jmc-has-word-p "nose") (jmc-has-word-p "unittest"))))


;; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: EMACS LISP

(defun jmc-eval-to-here ()
    (interactive)
    (eval-region 0 (point)))
;  (global-set-key [f9] 'jmc-eval-to-here)


(defun jmc-eval-and-test ()
  (interactive)
  (let ((funcname (command-execute 'eval-last-sexp)))
    (message "eval: %s" (apply funcname ()))))

(when nil
  (defun jm-eval-last ()
    (interactive)
    (let ((funcname (command-execute 'eval-last-sexp)))
      (message "eval: %s" (apply funcname ())))))
  
(defun jmc-eval-and-test ()
  "Evaluate entire buffer, displaying output of print statements.
Run function 'test'.
"
  (interactive)
  (eval-buffer nil t)
  (if (functionp 'test)
      (message "test: %s" (test))
    (message "(no tests)")))


;; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: PYTHON/NOSE

(defcustom jmc-nose-postproc "" "Post-processor, if any.")
(setq jmc-nose-postproc	" 2>&1 | ~/bin/simplifyp -")
(setq jmc-nose-postproc	nil)
;; (setq jmc-nose-program "/usr/bin/nosetests")

(defcustom jmc-nose-exclude		nil  "Exclude these tests (regex)")
;; (setq jmc-nose-exclude		nil)

(defun jmc-nose (switches args)
  "Run 'nosetests' external process.
Uses global switches 'jmc-nose-switches', then switches and args passed in.
"
  (let ((nose-path 
	 ;; (if (functionp jmc-nose-program)
	 ;;     (funcall jmc-nose-program)
	 (jmc-nose-program))
	(nose-exclude
	 (if (stringp jmc-nose-exclude)
	     (format "--exclude='%s'" jmc-nose-exclude))))
    (jmc-make 
     (format (concat 
	      nose-path 
	      " " jmc-nose-switches 
	      " " nose-exclude
	      " " switches " " args
	      jmc-nose-postproc
	    )))))


;; XX
(defun jmc-find-test (path)
  (let ((name (file-name-nondirectory path)))
    (if (string= "test_" (substring name 0 5))
	name
      (concat "test/test_" name))))
;; TEST: (jmc-find-test "/abc/zoot.py") => "test/test_zoot.py"
;; TEST: (jmc-find-test "test_beer.py") => "test_beer.py"


;; XXXX:
(defun jmc-find (name)
  (or 
   (locate-file 
    name
    (list "../bin" "../../bin" "../../../bin")
    nil)
    name))
;; TEST (jmc-find "jmpylint") => 


(defun jmc-is-testfunc (fname)
  t) 
;; (split-string context "\\.")
(defun jmc-testfunc-at-point (&optional curfunc)
  "Current Python test function, or nil."
  (let ((context 	(python-current-defun)))
    (when context
      (let* ((context	(substring-no-properties context))
	     (parts	(split-string context "\\."))
	     (funcname	(car (last parts))))
	(if (string-match-p "^test" funcname)
	    (format "geopoi.%s" context)))))) ;; XXXXX
    
;;;   (let ((context (python-current-defun)))
;;;     (if context
;;;       (let* ((context	(substring-no-properties context))
;;; 	     (outername	(car (split-string context "\\."))))
;;; 	(unless (eq nil (string-match-p "^test" (downcase outername)))
;;; 	  outername)))))
;; (global-set-key (kbd "<kp-home>") (lambda () (interactive) (message (jmc-nose-current))))

;; .................................................. interactive

(defun jmc-nose-buildout ()
  "Run Buildout's version of nosetests on tests for current buffer."
  (interactive)
    (if (jmc-py-has-tests-p)
	(jmc-nose-file)			; test current buffer
      ; test related file
      ; Ex: "beer.py" => test "test/test_beer.py"
    (jmc-nose "" (jmc-find-test (buffer-file-name)))))
;; TEST: (jmc-nose-buildout)

(defun jmc-nose-tree ()
  "Run nosetests on tree."
  (interactive)
  (jmc-nose "--exclude=jm" ""))

;; (defun jmc-nose-tree-most ()
;;   "Run nosetests on tree, except for a few files."
;;   (interactive)
;;   (jmc-nose (format "--exclude='%s'" jmc-nose-exclude) ""))


; Ex: "nosetests -sv file.py:TestClass.testFunc"
;
(defun jmc-nose-thisfunc (&optional args)
  "Run the test function under the cursor, or all tests in file, or all tests in related file."
  (interactive)
  (let ((testfunc (jmc-nose-current)))
    (cond 
     (testfunc				; function under cursor
      (jmc-nose (or args "")
		(format "%s:%s" 
			(jmc-py-testpath (buffer-file-name))
			testfunc)))
     ((jmc-py-has-tests-p)		; functions in file
      (jmc-nose-file))
     (t					; functions in related file (beer.py => test/test_beer.py)
      (jmc-nose "" (jmc-find-test (buffer-file-name)))))))

(defun jmc-nose-file ()
  "Run all tests in this file."
  (interactive)
  (if (jmc-py-has-tests-p)		; functions in file
       (jmc-nose "" (buffer-file-name))
    ;; functions in related file
    (jmc-nose "" (jmc-find-test (buffer-file-name)))))

;; XXX: cf jmc-py-format-import
(defun jmc-nose-module (srcpath)
  "Module for this source file.
Ex: mod1/mod2/code.py => 'mod1.mod2.code'
Ex: mod1/mod2/test/test_code.py => 'mod1.mod2.code'
"
  (let* ((parts 	(last (split-string 
			       (file-name-sans-extension srcpath) "/") 3))
	 (modname	(car (last parts))))
    (format "%s.%s.%s"
	    (car parts) 		; heh
	    (nth 1 parts) 
	    (nth 2 parts))))
;;
;; (jmc-nose-module "/x/a/b/c.py") "a.b.c"

(defun test () (interactive) (message (jmc-nose-module "/x/a/b/c.py")))
(local-set-key [kp-enter] 'test)

    
  



;; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: AWESOME WM

(defun jmc-awesome-reload-newest ()
  (interactive)
  (jmc-make "pkill -HUP -n awesome"))

(defalias 'jmc-awesome-reload 'jmc-awesome-reload-newest)


;; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: WEB

(defun jmc-reload-browser ()
  "Save buffers, reload current Firefox page."
  (interactive)
  (require 'moz)
  (save-some-buffers t)
  (save-window-excursion
    (with-temp-buffer
      (insert "BrowserReload();")
      (moz-send-defun))))


(defun jmc-dotest-web ()
  (jmc-reload-browser))


;; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: PYTHON

;; X: local?
(set-variable 'jmc-python-command python-python-command)
(set-variable 'jmc-python-command "/home/johnm/src/geodelic/bin/python")

(defun jmc-py-run ()
  "Syntax check this Python source (run this file using 'python')"
  (interactive)
  (jmc-make (format "%s %s" jmc-python-command (buffer-file-name))))

; XXX:
(defun jmc-django-test (testname)
  "Run Django's 'manage.py test' on a file or single class."
  (interactive)
  (jmc-make 
   (format "cd %s ; bin/run_tests %s"
	   project-dir
	   testname)))

(defun jmc-django-test-function ()
  (interactive)
  (let ((testfunc (jmc-testfunc-at-point)))
    (if testfunc
	(jmc-django-test testfunc))))
	
(defun jmc-django-restart ()
  "Restart Django"
  (interactive)
  (jmc-make (format "cd %s ; make -C johnm restart" "/home/johnm/src/geodelic")))

(defun jmc-django-test-thisclass ()
  (interactive)
  (jmc-django-test (jmc-nose-current)))




;; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: COMPILING

(defun jmc-make (newcommand)
  (interactive)
  (setq compile-command newcommand)
  (jmc-make-temp newcommand))

;; XXX: replace with compile-recompile
(defun jmc-make-temp (&optional newcommand)
  "Save buffers, compile, then restore old compile command."
  ;; 'compile-command' *shadows* customvar
  (interactive)
  (save-some-buffers t)
  (let ((compile-command (or newcommand compile-command)))
    (compile compile-command)))

;; TEST (jmc-make-temp "echo beer") (describe-variable 'compile-command)

(defun jmc-make-recompile ()
  "Save buffers, recompile."
  (interactive)
  (save-some-buffers t)
  (recompile))

(defun jmc-make-fast ()
  "Compile and test quickly, using 'make test-fast'"
  (interactive)
  (jmc-make "make test-fast"))


(defun jmc-make-test ()
  "Compile and test, using 'make test' or previous command."
  (interactive)
  (jmc-make (if compile-command compile-command "make test")))


(defun jmc-make-cmake ()
  "Compile using cmake."
  (interactive)
  (jmc-make "cmake CMakeLists.txt && make"))


;; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: GENERAL

;; (defun jmc-next ()
;;   "Jump to the next message.
;; From compilation (next-error), flymake-goto-next-error, or XX.
;; Jump to end of last unit test definition.
;; Debugger: next.
;; "
;;   (interactive)
;;   (cond 
;;    ((string= major-mode "gud-mode")		(gud-next))
;;    ((get-buffer-window "*compilation*")		(next-error)) ; Compilation visible
;;    ((assq 'flynote-mode minor-mode-alist)	(flynote-next))
;;    (t						(next-error))))


(defun jmc-test-fast ()
  "Run tests in this file, or run it."
  (interactive)
  (if (jmc-py-has-tests-p)
      (jmc-nose-file)
    (jmc-py-run)))


; flymake-err-info 



(defun jmc-pychecker (&optional switches args)
  (interactive)
  (jmc-make 
   (format "pychecker --stdlib --unreachable --quiet %s" (buffer-file-name))))
;; (define-key python-mode-map [kp-home] 'jmc-pychecker)


  

(provide 'jmcompile)



;; ################################################################ HISTORICAL


(defun jmc-py-fix-error ()
;; "Undefined variable 'os'"
)

(defun jmc-goto-last-import ()
  "Jump cursor to end of buffer's 'import' block."
  (interactive)
  (goto-char 0)
  (word-search-forward "import")
  (forward-paragraph-nomark))
; (global-set-key [kp-delete] 'jmc-goto-last-import)


(if (functionp 'apply-partially)
    (defalias 'jmc-nose-program		
      (apply-partially 'jmc-find "nosetests"))
  (defun jmc-nose-program ()
    (jmc-find "nosetests")))

;; (jmc-nose-program) => "nosetests"

(defcustom jmc-nose-switches 		"-sv" 
  "args for all 'nose' tests:
-s	don't capture stdout, print it
-v	verbose
--stop	stop after first error/failure
")
;; (setq jmc-nose-switches "-sv --stop --nologcapture")
;; (setq jmc-nose-switches "-sv")

(defcustom jmc-nose-thisfunc-switches	"%s:%s"
  "extra args for 'thisfunc' tests")
(defcustom jmc-nose-file-switches	"-a '!slow'"
  "extra args for 'file' tests")

;; (setq jmc-nose-switches "-sv --pdb-failures")


(defun jmc-py-testpath (srcpath)
  "Find test file for current file.
Example: 'zoot.py' will return 'test_zoot.py' if it exists, or 'zoot.py'.
XX: assumes in same directory."
  (let ((testpath (concat "test_" (file-name-nondirectory srcpath))))
    (if (file-exists-p testpath)
	testpath
      srcpath)))
;;TEST: (string= "test_fast.py" (jmc-py-testpath "fast.py"))

(defun jmc-dotest ()
  (interactive)
  (if (string= "python-mode" major-mode)
      (cond 
       ((eq jmc-direction 'python)	(jmc-nose-thisfunc))
       ((eq jmc-direction 'project)	(jmc-make-recompile)))
;; other modes:
    (jmc-make-recompile)))
  ;; (re-search-backward "def test" nil t)
  ;; (forward-paragraph))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REST/S5 SLIDES

(when nil
  (defun jmc-rst-export ()
    (interactive)
    (jmc-make "rst2s5 messageq.rst messageq.html"))
  ;;   (jmc-make "rst2html messageq.rst > messageq.html"))
  (defun jmc-rst-mode-hook ()
    (define-key rst-mode-map [kp-enter] 'jmc-rst-export)))


;; :::::::::::::::::::::::::::::::::::::::::::::::::: Python Debugger (PDB/GUD)

(defvar jmc-pdb-funcname nil "howdy")
(defvar jmc-pdb-buffer nil "Buffer that contains test function to run PDB on.")

(defun jmc-pdb ()
  (interactive)
  (save-some-buffers t)
  (let* ((pdb_format 	"%s -s --pdb --pdb-failures %s:%s")
	 (prog_path	(jmc-find "nosetests"))
	 (srcname	(buffer-file-name jmc-pdb-buffer)))
    (unless jmc-pdb-funcname
      (jmc-pdb-setfunc-raw))
    (when jmc-pdb-funcname
      (pdb (format pdb_format prog_path srcname jmc-pdb-funcname)))))

(defun jmc-pdb-setfunc-raw ()
  (setq jmc-pdb-buffer	(current-buffer)
	jmc-pdb-funcname (jmc-nose-current)))

(defun jmc-pdb-setfunc ()
  (interactive)
  (message (jmc-pdb-setfunc-raw))
  (jmc-pdb))

(defun jmc-pdb-toggle ()
  (interactive)
  (beginning-of-line)
  (if (looking-at "[^\n]*set_trace")
      (progn
	(beginning-of-line)
	(if (re-search-forward " *\\(#\\)" nil t)
	    (delete-backward-char 1)
	  (insert "#")))
    (progn
      (insert-for-yank "import pdb ; pdb.set_trace()\n")
      (previous-line)))
  (indent-for-tab-command))

;; (global-set-key (kbd "S-<kp-decimal>") 	'jmc-pdb-toggle)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HACKS

(defun tags-search-python-test (regexp)
  "Search through Python test files for REGEXP."
  (interactive "sTags search for test (regexp): ")
  (if (and (equal regexp "")
	   (eq (car tags-loop-scan) 're-search-forward)
	   (null tags-loop-operate))
      ;; Continue last tags-search as if by M-,.
      (tags-loop-continue nil)
    (setq tags-loop-scan `(re-search-forward ',regexp nil t)
	  tags-loop-operate nil)
    (tags-loop-continue (or file-list-form t))))

;; Relationship

(global-set-key (kbd "M-t") 'grep-find-python-test)


(when nil
  (defun jmc-py-thing-at-point ()
    (interactive)
    (python-beginning-of-defun)
    (message (python-first-word))))
  

(defun jmc-py-format-import (symbol buffer)
  (save-excursion
    (let* ((buf (find-tag-noselect symbol))
	   (parts (split-string 
		   (file-name-sans-extension (buffer-file-name buffer)) "/"))
	   (parts (last parts 3)))
      (format "from %s.%s.%s import %s"
	      (car parts)
	      (nth 1 parts)
	      (nth 2 parts)
	      ;; XXX (string-join butlast zparts 1) "."
	      symbol))))

(defun jmc-py-copyarg ()
  "Point at word, edit so it becomes a copy argument
Ex: zoot(all)  => zoot(all=all)
"
  (interactive)
  (let ((argname (thing-at-point 'symbol)))
    (forward-word)
    (insert "=")
    (insert argname)
    (forward-word)
    (backward-char)
    ;(indent-for-tab-command
    ))
;; (global-set-key [kp-home] 'jmc-py-copyarg)
;; zoot(all) => zoot(all=all, \n )


(defun jmc-py-insert-import ()
  (interactive)
  (let ((importline (jmc-py-format-import (thing-at-point 'word) (current-buffer))))
    (beginning-of-line)
    (insert-for-yank (concat importline "\n"))))

(defun jmc-insert-goodvalue ()
  "Search for assertion in compilation buffer, paste into
current (code) buffer."
  (interactive)
  (let ((goodvalue
	 (save-excursion
	   (set-buffer (get-buffer "*compilation*"))
	   (goto-char (point-max))
	   (when (re-search-backward "AssertionError: \\(.+\\) != " nil t)
	     (match-string 1)))))
    (when goodvalue
      (insert-for-yank goodvalue))))


;; (defun jmc-eval-and-test ()
;;   (interactive)
;;   (eval-last-sexp))
;; (local-set-key [kp-enter] 'jmc-eval-and-test)

(when nil
  (global-set-key [C-kp-subtract] 'jmc-py-copyarg)
  (global-set-key [kp-right] 'jmc-insert-goodvalue))


;;  (jmc-make (format "cd %s ; bin/pctl restart app" "/home/johnm/src/geodelic")))
;; (define-key python-mode-map [kp-add]  'jmc-django-test-function)
;;     (format "cd %s ; bin/run_tests %s 2>&1 | egrep -v '^(Install|Creat|\\+)'"

;; (defun jmc-nose-current-class (&optional curfunc)
;;   "Current Python test class, or nil."
;;   (let ((context (python-current-defun)))
;;     (if context
;;       (let* ((context	(substring-no-properties context))
;; 	     (outername	(car (split-string context "\\."))))
;; 	(unless (eq nil (string-match-p "^test" (downcase outername)))
;; 	  outername)))))

;; (defun jmc-python-hook ()
;;    (define-key python-mode-map [kp-enter] 'jmc-py-test-fast)
;;    (define-key python-mode-map [kp-add] 'jmc-make-test))
;; (add-hook 'python-mode-hook 'jmc-python-hook)


