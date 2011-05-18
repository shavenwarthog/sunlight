;; chromedev.el -- talk to Google Chrome debugger port
;; http://code.google.com/p/chromedevtools/wiki/ChromeDevToolsProtocol

;; google-chrome --remote-shell-port=9222 
;; --user-data-dir=DIR --incognito



(defvar chromedev-proc nil "ding")

(defun chromedev-send (str)
  (process-send-string chrome-proc str))

(defun chromedev-format-request (payload &optional tool destination)
  (let ((CRLF "\r\n"))
    (concat (format "Content-Length:%s" (length payload)) CRLF
	    (format "Tool:%s" (or tool "DevToolsService")) CRLF
	    (if destination
		(format "Destination:%s%s" destination CRLF))
	    CRLF
	    payload)))

(defun chromedev-version ()
  (chromedev-send (chromedev-format-request "{\"command\":\"version\"}")))

(defun chromedev-list-tabs ()
  (chromedev-send (chromedev-format-request "{\"command\":\"list_tabs\"}"))

(defun chromedev-reload (tabnum)
  (chromedev-send 
   (chromedev-format-request 
    (concat "{\"command\":\"evaluate_javascript\", "
	    "\"data\": \"window.location.reload()\""
	    "}")
    "V8Debugger" tabnum)))

(defun chromedev-connect ()
  (setq chrome-proc 
	(open-network-stream "Chrome" "*chrome*" "localhost" 9222))
  (process-send-string chrome-proc (concat "ChromeDevToolsHandshake" CRLF)))
