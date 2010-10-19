

;; (defvar compilation-mode-font-lock-keywords
;;    '(;; configure output lines.
;;      ("^[Cc]hecking \\(?:[Ff]or \\|[Ii]f \\|[Ww]hether \\(?:to \\)?\\)?\\(.+\\)\\.\\.\\. *\\(?:(cached) *\\)?\\(\\(yes\\(?: .+\\)?\\)\\|no\\|\\(.*\\)\\)$"
;;       (1 font-lock-variable-name-face)
;;       (2 (compilation-face '(4 . 3))))
;;      ;; Command output lines.  Recognize `make[n]:' lines too.
;;      ("^\\([[:alnum:]_/.+-]+\\)\\(\\[\\([0-9]+\\)\\]\\)?[ \t]*:"
;;       (1 font-lock-function-name-face) (3 compilation-line-face nil t))
;;      (" --?o\\(?:utfile\\|utput\\)?[= ]?\\(\\S +\\)" . 1)
;;      ("^Compilation \\(finished\\).*"
;;       (0 '(face nil message nil help-echo nil mouse-face nil) t)
;;       (1 compilation-info-face))
;;      ("^Compilation \\(exited abnormally\\|interrupt\\|killed\\|terminated\\|segmentation fault\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
;;       (0 '(face nil message nil help-echo nil mouse-face nil) t)
;;       (1 compilation-error-face)
;;       (2 compilation-error-face nil t)))
;;    "Additional things to highlight in Compilation mode.
;; This gets tacked on the end of the generated expressions.")

(insert (string (nth 3 compilation-mode-font-lock-keywords)))

(when nil
  ("^Compilation \\(finished\\).*"
   (0 '(face nil message nil help-echo nil mouse-face nil) t)
   (1 compilation-info-face)))

;; http://www.emacswiki.org/emacs/FontLockKeywords
;; (REGEXP . FACE)			Highlight REGEXP with FACE
;; (REGEXP N FACE)			Highlight group N in REGEXP with FACE
;; (REGEXP (N1 FACE1) (N2 FACE2) (N3 FACE3) …)
;; 		Highlight group Ni in REGEXP with FACE
;;  MATCHER
;;  (MATCHER . SUBEXP)
;;  (MATCHER . FACENAME)
;;  (MATCHER . HIGHLIGHT)
;;  (MATCHER HIGHLIGHT ...)
;;  (eval . FORM)

;; (font-lock-add-keywords 'emacs-lisp-mode
;;   '(("foo" . font-lock-keyword-face)))
;; It makes “foo” a keyword in EmacsLisp mode.

(when nil
  (font-lock-add-keywords 'compilation-mode
			  '(("beer" . font-lock-keyword-face))))

;; -error=pink bold
;; -warning = orange bold
;; compilation-info=green bold
;; -line-number -column-number -message=underline

(font-lock-add-keywords
 'compilation-mode
 '(("beer" . 'compilation-warning)
   ("image" . 'compilation-info)
   
   ;; yellow boxy generic markers:
   ("} != {" 'hi-yellow)
   ("\) != \(" 'hi-yellow)
   ("\\] != \\[" 'hi-yellow)))

(defface jm-invisible 
  '((t :foreground "green4" 
       :invisible t 
       )) "red")

(font-lock-add-keywords
 'compilation-mode
 '(("gin" . 'compilation-warning)
   ("woo" . 'jm-invisible)))

(put-text-property 1 20 'invisible t)
(setq buffer-invisibility-spec t)
(setq selective-display t)

(defun re-zoot ()
  (interactive)
  (goto-char (point-min))
  (when (re-search-forward (concat "\\W\\(" "water" "\\)\\W") nil t)
    (put-text-property 
     (match-beginning 1) (match-end 1)
     'invisible t)))

(defun zoot ()
  (interactive)
  (let* ((end (point-max))
	 (start (text-property-any (point-min) end 'face nil))
	 next)
    (while start
      (setq next (next-single-property-change start 'face))
      (put-text-property start next 'face 'compilation-warning)
      (setq start (text-property-any next end 'face nil)))))


(global-set-key (kbd "<kp-insert>") 'zoot)
