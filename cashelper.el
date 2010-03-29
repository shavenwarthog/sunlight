;; make dull Cassandra output fade, allowing relevant bits to be more readable
;; see also: (list-faces-display)
;;
(require 'hi-lock)
(global-hi-lock-mode 1)

(defface hi-dim '((t :foreground "gray20" :background nil)) "dark gray")

(defun cassandra-be-good-hook ()
  (interactive)
  (hi-lock-set-file-patterns 
   '(("timestamp=.+?," (0 'hi-dim t))
     ("super_column=None" (0 'hi-dim t))
     ("[Cc]olumn[A-z]*" (0 'hi-dim t))
     ("Col[A-Za-z]+," (0 'hi-dim t))
     ("[Cc]onsis.+?," (0 'hi-dim t))
     )))

(add-hook 'compilation-mode-hook 'cassandra-be-good-hook)

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

