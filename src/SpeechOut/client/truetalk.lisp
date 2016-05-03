;;;
;;; truetalk.lisp
;;;
;;; George Ferguson, ferguson@cs.rochester.edu, 17 Mar 1997
;;; Time-stamp: <Wed May 14 16:56:30 EDT 1997 ferguson>
;;;

(in-package "GENERATOR")

(defvar *truetalk-command* "/u/trains/97/3.0/bin/tspeechout -audio /dev/audio"
  "Command to run truetalk.")

(defvar *truetalk-stream* nil
  "Stream connected to/from the truetalk process.")

(defun truetalk-start ()
  "Starts the truetalk subprocess and opens *TRUETALK-STREAM*."
  (format *trace-output* "Starting truetalk with command \"~A\"...~%"
	  *truetalk-command*)
  (setq *truetalk-stream*
    (excl:run-shell-command *truetalk-command*
			    :input :stream :output :stream :wait nil))
  ;; Wait for it to send ready message
  (format *trace-output* "Waiting for truetalk READY message...~%")
  (prog1 (read *truetalk-stream*)
    (format *trace-output* "Truetalk ready.~%")))

(defun truetalk-kill ()
  "Send the EXIT message to truetalk and closes *TRUETALK-STREAM*."
  (cond ((not *truetalk-stream*)
	 (format *error-output* "Truetalk is not running.~%"))
	(t
	 (format *trace-output* "Stopping truetalk...~%")
	 (format *truetalk-stream* "(request :content (exit 0))~%")
	 (finish-output *truetalk-stream*)
	 (close *truetalk-stream*)
	 (setq *truetalk-stream* nil))))

(defun truetalk-send (str &key dont-wait)
  "Sends the given STR to truetalk as (REQUEST :content (SAY STR)).
If DONT-WAIT is NIL (the default), then this function waits for a reply
from truetalk and returns it. If DONT-WAIT is T, then the reply (if any)
will still be waiting on *TRUETALK-STREAM*."
  ;; Start truetalk if not started yet
  (when (not *truetalk-stream*)
    (truetalk-start))
  ;; Send message
  (format *trace-output* "Sending to truetalk:~%  ~S~%" str)
  (format *truetalk-stream* "(REQUEST :CONTENT (SAY ~S))~%" str)
  (finish-output *truetalk-stream*)
  ;; Unless we were told not to wait...
  (unless dont-wait
    ;; ...Wait for reply and return its content
    (let ((reply (read *truetalk-stream*)))
      (format *trace-output* "Received from truetalk:~%  ~S~%" reply)
      reply)))
