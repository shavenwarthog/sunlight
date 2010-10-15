
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

(load "~/src/sunlight/jmc_python_pdb")	;XXX

;; (defun jmc-has-word-p (word)
;;   "True if WORD found in next few lines."
;;   (not (eq nil (word-search-forward word 1000 t))))

(defun jmc-py-has-tests-p ()
  "Return true if current buffer contains unit tests.
IE: search for 'nose' or 'unittest' imports."
  (save-excursion
    (goto-char 0)
    (re-search-forward "\\(nose\\|unittest\\|def test\\)" nil t)))

;; (global-set-key (kbd "<C-kp-home>") '(lambda () (interactive)
;; (message "ding: %s" (jmc-py-has-tests-p))))

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
;; (setq jmc-nose-postproc	" 2>&1 | ~/bin/simplifyp -")
(setq jmc-nose-postproc	nil)
;; (setq jmc-nose-program "/usr/bin/nosetests")

(defcustom jmc-nose-exclude		nil  "Exclude these tests (regex)")
;; (setq jmc-nose-exclude		nil)

(defun jmc-nose (switches args)
  "Run 'nosetests' external process.
Uses global switches 'jmc-nose-switches', then switches and args passed in.
"
  (let ((nose-path	(jmc-nose-program))
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


;; (defun jmc-is-testfunc (fname)
;;   t) 
;; (split-string context "\\.")

(defun jmc-pdb-enabled-near-point ()
  (save-excursion
    (beginning-of-line -10)
    (re-search-forward "^[^#]+set_trace" (line-end-position 20) 'noerror)))
;; pdb.set_trace

;; (local-set-key 
;;  (kbd "<kp-home>") 
;;  (lambda ()
;;    (interactive)
;;    (message "woo: %s" (jmc-pdb-enabled-near-point))))
				   
;; XX gauche but workable
;; (defun jmc-all-testcases ()
;;   (let ((result 
;;   (while (re-search-forward "class \\(.+?\\)\(.*TestCase" nil t)
    
;; class Zoot_Test(TestCase)

  
    
;; Django:
(defun jmc-django-appname (&optional srcpath)
  (let ((srcpath (or srcpath buffer-file-name)))
    ; geopoi et al:
    (cond
     ((string-match "/apps/\\(.+?\\)/" srcpath)      (match-string-no-properties 1 srcpath))
     ((string-match "/victoryM/" srcpath)
      "victoryM")
     ((string-match "/users/" srcpath)
      "users"))))

;; from server/tests.py:  format=appname: module
;;
;;;     'elmer':    elmer.tests,
;;;     'apis':     api_tests,
;;;     'users':    users.tests,
;;;     'shindig':  shindig.tests,
;;;     'mongo':    mongo.test,
;;;     'learning': learning.tests,
;;;     'search':   search.tests,
;;;     'victoryM': victoryM.tests,
;;;     'openpub_users': openpub.apps.users.tests,
;;;     'clustering': clustering.tests,
;;;     'auth_wrap': auth_wrap_tests,
;;;     'sjson': sjson.tests,
;;;     'mongo_publish' : mongo.publishing.tests, 
;;;     'mongo_user_migrate': mongo.migration.tests
     

;; (jmc-django-appname "/apps/zoot/allures.py") => "zoot"
;; (jmc-django-appname "hmm") => nil

(defun jmc-testfunc-at-point (&optional context srcpath)
  "Current Python test function, class, or nil."
  (let ((context 	(or context (python-current-defun)))
	(srcpath	(or srcpath buffer-file-name)))
    (when context
      (let* ((context	(substring-no-properties context))
	     (parts	(split-string context "\\."))
	     (funcname	(car (last parts))))
	(cond 
	 ((string-match-p "^test" funcname)	    context)
	 ((string-match-p "/t" srcpath)	    context) ;XXX
	 (t					    nil))))))
;; (jmc-testfunc-at-point "Zoot.testZoot") => "Zoot.testZoot"
;; (jmc-testfunc-at-point "testZoot") => "testZoot"
;; (jmc-testfunc-at-point "zoot") => nil
;; (jmc-testfunc-at-point "zoot" "/tests.py") => "zoot"
(when nil
  (global-set-key 
   (kbd "<kp-divide>")
   (lambda () (interactive) 
     (message "ding: func=%s class=%s" (jmc-testfunc-at-point) (jmc-testclass-at-point))))
  (global-set-key 
   (kbd "S-<kp-divide>")
   (lambda () (interactive) 
     (message "ding2: %s" (jmc-testfunc-info)))))


(defun jmc-testclass-at-point (&optional context srcpath)
  (let ((testfunc (jmc-testfunc-at-point context srcpath)))
    (if testfunc
	(let ((parts (split-string testfunc "\\.")))
	  (if (> (length parts) 1)
	      (combine-and-quote-strings
	       (butlast parts)
	       ".")
	    testfunc)))))

;; (jmc-testclass-at-point "ace.Zoot.testZoot") => "ace.Zoot"
;; (jmc-testclass-at-point "ace" "/test.py") => "ace"




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
(defun jmc-nose-test-function (&optional args)
  "Run the test function under the cursor, or all tests in file, or all tests in related file."
  (interactive)
  (let ((testfunc (jmc-testfunc-at-point)))
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
;; (jmc-nose-module "/x/a/b/c.py") => "a.b.c"

    
;; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: PYTHON

;; X: local?
(set-variable 'jmc-python-command python-python-command)
(set-variable 'jmc-python-command 
	      (format "%s/bin/python" project-dir))

(defun jmc-py-run ()
  "Syntax check this Python source (run this file using 'python')"
  (interactive)
  (jmc-make (format "%s %s" jmc-python-command (buffer-file-name))))


;; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: PROJECT XX

; XXX:
(defun project-test-helper (srcpath testname)
  (let ((appname (jmc-django-appname srcpath)))
    ;; (if (stringp appname)
    (format "%s%s" appname (if testname (concat "." testname) ""))))

;;; (jmc-django-appname "/apps/geopoi/tests.py") => "geopoi"
;;; (project-test-helper "/apps/geopoi/tests.py" "x") => "geopoi.x"
;;; (project-test-helper "/apps/exp/t_misc.py" "x") => "exp.x"

(defun project-active (srcpath)
  (string-match project-dir srcpath))	; XX: absdir

;; (project-active "beer") => nil
;; (project-active "/home/johnm/src/geodelic/server/apps/geopoi/tests.py") => 0 XX

;; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: DJANGO
;;
;; use normal "unittest" framework, run tests via external "run_tests" script

(set-variable 'jmc-django-restart-command "cd %s ; make -C johnm restart")
(set-variable 'jmc-django-test-command "cd %s ; johnm/run_tests %s")

(defun jmc-django-restart ()
  "Restart Django"
  (interactive)
  (jmc-make (format jmc-django-restart-command project-dir)))

(defun jmc-django-test (testpath testname)
  "Run Django's 'manage.py test' on a file or single class."
  (interactive)
  (jmc-make
   (format jmc-django-test-command
	   project-dir
	   (project-test-helper testpath testname))))

(defun jmc-django-test-function ()
  (interactive)
  (let ((testfunc (jmc-testfunc-at-point)))
    (if testfunc
	(jmc-django-test buffer-file-name testfunc))))

(defun jmc-django-test-class ()
  (interactive)
  (let ((testclass (jmc-testclass-at-point)))
    (if testclass
	(jmc-django-test buffer-file-name testclass))))

;; XXXX:
(defun jmc-django-test-file ()
  (interactive)
  (jmc-django-test buffer-file-name nil))
	
(defun jmc-django-test-app ()
  (interactive)
  (jmc-django-test buffer-file-name nil))

;; XXXX:
;; (defun jmc-django-test-all-classes (testpath testname)
;;   (interactive)
;;   (let ((appname (jmc-django-appname srcpath)))
;;     (jmc-make
;;      (format jmc-django-test-command
;; 	     project-dir
;; 	     (project-test-helper testpath testname))))
	
;; (defun jmc-django-test-class ()
;;   (interactive)
;;   (jmc-django-test buffer-file-name (jmc-nose-current)))


;; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: PER-PROJECT

(defun jmc-custom (projectfunc generalfunc)
  (interactive)
  (message "custom: %s %s %s" (project-active buffer-file-name) projectfunc generalfunc)
  (funcall (if (project-active buffer-file-name) projectfunc generalfunc)))

;; (jmc-custom nil '(lambda () (message "woohoo"))) => "woohoo"
;;  (funcall (if (project-active) projectfunc generalfunc)))





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

(defalias 'jmc-next 'next-error)
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

(defun jmc-pyflakes (&optional switches args)
  (interactive)
  (jmc-make 
   (format "pyflakes %s" (buffer-file-name))))
;; (define-key python-mode-map [kp-home] 'jmc-pyflakes)


  

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
(defun jmc-nose-program ()
  "nosetests"
  ;; "/usr/bin/nosetests"
  ;; "/home/johnm/local/bin/nosetests"
  )

;; (jmc-nose-program) => "nosetests"

(defcustom jmc-nose-switches 		"-sv" 
  "args for all 'nose' tests:
-s	don't capture stdout, print it
-v	verbose
--stop	stop after first error/failure
")
;; (setq jmc-nose-switches "-sv --stop --nologcapture")
;; (setq jmc-nose-switches "-v --stop --logging-filter=ignore")

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

(defun jmc-find-tag (tagname &optional next-p regexp-p)
  "Find tag, reloading tags file if it's been updated."
  (interactive (find-tag-interactive "Find tag: "))
  (visit-tags-table tags-file-name)
  (find-tag tagname next-p regexp-p))

;;   (tags-reset-tags-tables)
;; (global-set-key (kbd "M-'") 'jmc-find-tag)


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


;; enter=redo, control-enter=test function, add=test class
;;
;; (defun jmc-use-django (&optional path)
;;   nil)


(defun jmc-key-test-function ()
  (interactive) 
  (jmc-custom 'jmc-django-test-function 'jmc-nose-test-function))

(defun jmc-key-test-class ()
  (interactive)
  (jmc-custom 'jmc-django-test-class nil))

(defun jmc-key-test-file ()
  (interactive)
  (jmc-custom 'jmc-django-test-file
	      '(lambda () (jmc-nose "" (buffer-file-name)))))


;; XX:
(defun jmc-key-testdir ()
  (interactive)
  (jmc-nose "" (file-name-directory (buffer-file-name))))
  
;; (global-set-key 
;;  [kp-multiply]
;;  (lambda () (interactive) 
;;    (if (string-match-p "/test" buffer-file-name)
;;        (jmc-django-test-file)
;;      (jmc-django-restart))))

(defun jmc-hi-lock ()
  (interactive)
  (hi-lock-line-face-buffer "^\\s+logging.info+?(" 'hi-yellow)
  (hi-lock-line-face-buffer "^\\s+logging.error+?(" 'hi-yellow))

(defun project-which-app (&optional path)
  (let ((path (or path buffer-file-name)))
    (when (or (string-match "/\\(openpub\\)/" path)
	      (string-match "/apps/\\(.+?\\)/" path))
      (match-string-no-properties 1 path))))

;; (project-which-app "zoot") => nil
;; (project-which-app "/apps/beer") => nil
;; (project-which-app "/apps/beer/yum.py") => "beer"
;; (project-which-app "/openpub/apps/exp/beer.py") => "openpub"
      
(defun jmc-python-which-func ()
  (let ((app (project-which-app)))
    (when app
      (format "%s %s" app (or (python-which-func) "<file>")))))

(defun jmc-python-hook ()
  (interactive)
  (add-to-list 'which-func-functions 'jmc-python-which-func)
  (jmc-hi-lock))

(add-hook 'python-mode-hook 'jmc-python-hook)
; (custom-add-option 'python-mode-hook 'jmc-python-hook)

(when nil
  (custom-add-option 
   (lambda () (add-to-list 'which-func-functions 'jmc-python-which-func))))
;;  (add-hook 'which-func-functions 'python-which-func nil t)

  