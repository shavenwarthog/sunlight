;; :::::::::::::::::::::::::::::::::::::::::::::::::: PROJECT

;; left to right: 1=code, 2=test, 3=auxillary
;; control-1 = set source code to what's in current buffer
(when t
  (progn
    (global-set-key [kp-end] 'jmc-project-open-code)
    (global-set-key [kp-down] 'jmc-project-open-test)
    (global-set-key [C-kp-end] 'jmc-project-set-name)))
  ;; (progn
  ;;   (global-set-key [kp-left] 'jmc-project-open-spec)
  ;;   (global-set-key [kp-next] 'jmc-project-open-ref)))




;; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: PROJECT

(defcustom jmc-project-dir nil "woo")
(defcustom jmc-project-name nil "Project name, like 'basset'.")
(defcustom jmc-project-ref nil "Reference code for the project.")

(defun jmc-project-set-name (&optional path)
  "Set this buffer's file to be the main project file."
  (interactive)
  (setq path (or path (buffer-file-name)))
  ;; XXX (setq jmc-nose-program 'jmc-buildout-find)
  (setq jmc-project-dir (file-name-directory path))
  (let* ((path (or path (buffer-file-name)))
	 (name (file-name-sans-extension 
		(file-name-nondirectory path))))
    (setq jmc-project-name name)
    (message (format "project: %s in %s" name jmc-project-dir))))
;; TEST: (jmc-project-set-name)
;; X: make sticky, Customize?
;; X: project toplevel vs sourcedir vs testdir


(defun jmc-project-find (name)
  "Open buffer, or open file."
  (find-file (concat jmc-project-dir "/" name)))
;; (defun jmc-project-set-name ()
  
(defun jmc-project-open-code ()
  (interactive)
  (jmc-project-find (concat jmc-project-name ".py")))

(defun jmc-project-open-test ()
  (interactive)
  ;; XX open buffer
  (jmc-project-find (format "test/test_%s.py" jmc-project-name)))
(defun jmc-project-open-ref ()
  (interactive)
  (find-file-read-only jmc-project-ref))
(defun jmc-project-open-spec ()
  (interactive)
  (find-file jmc-project-spec))


(provide 'project)
