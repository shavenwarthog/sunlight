;; :::::::::::::::::::::::::::::::::::::::::::::::::: Python Debugger (PDB/GUD)

(defvar jmc-pdb-funcname nil "howdy")
(defvar jmc-pdb-buffer nil "Buffer that contains test function to run PDB on.")

;; XXXX
(when nil
  (set-variable 'gud-pdb-command-name "/home/johnm/src/geodelic/bin/run_tests"))

(defun jmc-nosetests-pdb ()
  (interactive)
  (save-some-buffers t)
  (let* ((pdb_format 	"%s -s --pdb --pdb-failures %s:%s")
	 (prog_path	(jmc-find "nosetests"))
	 (srcname	(buffer-file-name jmc-pdb-buffer)))
    (unless jmc-pdb-funcname
      (jmc-pdb-setfunc-raw))
    (when jmc-pdb-funcname
      (pdb (format pdb_format prog_path srcname jmc-pdb-funcname)))))

(set-variable 'jmc-django-test-command "cd %s ; johnm/run_tests %s")
;; (set-variable 'jmc-django-test-command "cd %s ; bin/run_tests %s")

;; (defalias 'jmc-pdb 'jmc-project-pdb)

;; (defun jmc-pdb-setfunc-raw ()
;;   (setq jmc-pdb-buffer	(current-buffer)
;; 	jmc-pdb-funcname (jmc-testfunc-at-point)))

;; (defun jmc-pdb-setfunc ()
;;   (interactive)
;;   (message (jmc-pdb-setfunc-raw))
;;   (jmc-django-test jmc-pdb-testpath

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



