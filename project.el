;; :::::::::::::::::::::::::::::::::::::::::::::::::: PROJECT

;; left to right: 1=code, 2=test; also 4=specification
;; control-1 = set source code to what's in current buffer
(when t
  (progn
    (global-set-key [kp-end] 'jmc-project-open-code)
    (global-set-key [kp-down] 'jmc-project-open-test)
    (global-set-key [kp-left] 'jmc-project-open-spec)
    (global-set-key [C-kp-end] 'jmc-project-set)
    (global-set-key [C-kp-down] 'jmc-project-set-test-path)))

;;(global-set-key [kp-1] 'jmc-project-open-code)
;;(global-set-key [C-kp-1] 'jmc-project-set)

;; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: PROJECT

;; project = top-level
;; current = source file name, with associated test file
;;
(defcustom jmc-project-top-dir nil "woo")
(defcustom jmc-project-source-dir nil "woo")
(defcustom jmc-project-source-path nil "woo")
(defcustom jmc-project-source-name nil "")
(defcustom jmc-project-test-path nil "")

(defun jmc-project-set (&optional path)
  "Set project name and directory based on current buffer's file path."
  (interactive)
  (let* ((path 	(or path (buffer-file-name)))
	 (name	(file-name-sans-extension 
		 (file-name-nondirectory path))))
    (setq jmc-project-name name
	  jmc-project-source-path path
	  jmc-project-source-dir (file-name-directory path)
	  jmc-project-test-path	nil)
    (message (format "project: %s in %s" name jmc-project-source-dir))))

(defun jmc-project-set-test-path (&optional path)
  ""
  (interactive)
  (setq jmc-project-test-path (or path (buffer-file-name))))

(defun jmc-project-find (name)
  "Open buffer, or open file."
  (if (file-exists-p name)
    (find-file name)))
;; (concat jmc-project-dir "/" name)))

;; XXXX
(defun jmc-project-open-code ()
  (interactive)
  (jmc-project-find
   (cond 
    (jmc-project-source-path	jmc-project-source-path)
    (jmc-project-name    	(format "%s.py" jmc-project-name)))))

(defun jmc-project-open-test ()
  (interactive)
  (jmc-project-find
   (cond 
    (jmc-project-test-path	jmc-project-test-path)
    (jmc-project-source-dir    	(format "%s/tests.py" jmc-project-source-dir)))))

(defun jmc-project-open-spec ()
  (interactive)
  (find-file jmc-project-spec))

;; (find-file 
;; (file-name-completion jmc-project-name "~/Documents/")


(provide 'project)
