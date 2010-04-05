;; make dull Cassandra output fade, allowing relevant bits to be more readable
;; see also: (list-faces-display)
;;
(require 'hi-lock)
(global-hi-lock-mode 1)

(defface hi-dim '((t :foreground "gray20" :background nil)) "dark gray")

(defvar custcompile-hi-lock nil "zoot")
(defun custcompile-hook ()
  (interactive)
  (when custcompile-hi-lock
    (hi-lock-set-file-patterns 
     '(("timestamp=.+?," (0 'hi-dim t))
       ("super_column=None" (0 'hi-dim t))
       ("[Cc]olumn[A-z]*" (0 'hi-dim t))
       ("Col[A-Za-z]+," (0 'hi-dim t))
       ("[Cc]onsis.+?," (0 'hi-dim t))
       ("x.expects.+?," (0 'compilation-info t)) ; bright green
       ("predicate=.+\)\)" (0 'hi-dim t))
       ("name='.*?'" (0 'button t)) ; underlined
       ("value='.*?'" (0 'button t)) ; underlined
       ("key='.*?'" (0 'compilation-error t)) ; pinkish
       ;; for Fudge:
       ("was unexpected; Expected:"	(0 'hi-pink t)) ; pink background
       (" #[0-9] "	(0 'hi-pink t)) ; pink background
       ))))
;; (defun cassandra-be-good-hook () (interactive))
(add-hook 'compilation-mode-hook 'custcompile-hook)

;; test text:
;; [KeySlice(columns=[ColumnOrSuperColumn(column=Column(timestamp=1269904700887173120,
;; name='ts1', value=''), super_column=None)], key='size'),
;; KeySlice(columns=[ColumnOrSuperColumn(column=Column(timestamp=1269904700873914880,
;; name='ts1', value='[0]'), super_column=None)], key='size:med'),
;; KeySlice(columns=[ColumnOrSuperColumn(column=Column(timestamp=1269904700881336064,
;; name='ts1', value=''), super_column=None)], key='status'),
;; KeySlice(columns=[ColumnOrSuperColumn(column=Column(timestamp=1269904700870554112,
;; name='ts1', value='[0]'), super_column=None)], key='status:ok'),
;; KeySlice(columns=[ColumnOrSuperColumn(column=Column(timestamp=1269904700882494976,
;; name='ts1', value=''), super_column=None)], key='title'),
;; KeySlice(columns=[ColumnOrSuperColumn(column=Column(timestamp=1269904700876520960,
;; name='ts1', value='[1]'), super_column=None)], key='title:story'),
;; KeySlice(columns=[ColumnOrSuperColumn(column=Column(timestamp=1269904700878963968,
;; name='ts1', value='[0]'), super_column=None)], key='title:toy')]

