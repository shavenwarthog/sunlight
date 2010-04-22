;; :::::::::::::::::::::::::::::::::::::::::::::::::: PROJECT

;; left to right: 1=code, 2=test; also 4=specification
;; control-1 = set source code to what's in current buffer
(when t
  (progn
    (global-set-key [kp-end] 'jmc-project-open-code)
    (global-set-key [kp-down] 'jmc-project-open-test)
    (global-set-key [kp-left] 'jmc-project-open-spec)
    (global-set-key [C-kp-end] 'jmc-project-set)
    (global-set-key [C-kp-down] 'jmc-project-set-testpath)))


;; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: PROJECT

;; project = top-level
;; current = source file name, with associated test file
;;
(defcustom jmc-project-top-dir nil "woo")
(defcustom jmc-project-source-dir nil "woo")
(defcustom jmc-project-source-path nil "woo")
(defcustom jmc-project-source-name nil "")
(defcustom jmc-project-testpath nil "")

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

(defun jmc-project-set-testpath (&optional path)
  ""
  (interactive)
  (setq jmc-project-testpath (or path (buffer-file-name))))

(defun jmc-project-find (name)
  "Open buffer, or open file."
  (find-file name))
;; (concat jmc-project-dir "/" name)))

;; XXXX
(defun jmc-project-open-code ()
  (interactive)
  (jmc-project-find (concat jmc-project-name ".py")))

;; XXXX
(defun jmc-project-open-test ()
  (interactive)
  (cond 
   (jmc-project-testpath	(jmc-project-find jmc-project-testpath))
   ;; (jmc-project-top-dir		(jmc-project-find 
   ;; 				 (format jmc-project-top-dir 
    
  (jmc-project-name    		(jmc-project-find 
				 (format "test/test_%s.py" jmc-project-name)))))

(defun jmc-project-open-spec ()
  (interactive)
  (find-file jmc-project-spec))

;; (find-file 
;; (file-name-completion jmc-project-name "~/Documents/")


(provide 'project)
