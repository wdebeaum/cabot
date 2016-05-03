;;;;
;;;; warn.lisp
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 24 Sep 2003
;;;; Time-stamp: <Wed Aug  1 15:17:41 EDT 2012 jallen>
;;;;

(in-package :llearner)

(defun da-warn (&rest args)
  (let ((msg (apply #'format nil args)))
    (logging:log-message :warn msg)
    (format *trace-output* "~&llearner: warning: ~A~%" msg)))

(defun da-debug (&rest args)
  (let ((msg (apply #'format nil args)))
    (logging:log-message :debug msg)
    (format *trace-output* "~&llearner: ~A~%" msg)))

(defvar *step* nil)
(defvar *trace-level* nil)

(defun set-debug-level (&key level)
  (case level
    (off (trace-off))
    (debug (trace-on 1))
    (otherwise (trace-on 2))))

(defun trace-on (n)
  (if (numberp n)
      (setq *trace-level* n)
    (da-warn "Step level must be a number")))

(defun trace-off nil
  (setq *trace-level* nil)
  (setq *step* nil))

(defun trace-msg (n &rest args)
  (when *trace-level*
    (when (>= *trace-level* n)
      (let ((msg (apply #'format nil args)))
	(format *trace-output* "~%LLEARNER: ~A" (concatenate 'string (case n (1 "") (2 "   ") (3 "     ") (4 "       "))
			       msg))
	
	(when *step* 
	  (format *trace-output* "~%LLEARNER:     at level ~S, change?:" *trace-level*)
	  (let ((x (read-line)))
	    (if (not (string= x ""))
		(let ((new (read-from-string x)))
		  (if (numberp new) (setq *trace-level* new)
		    (eval x))))))))
    (values)))

(defun trace-pprint (n msg x)
  (when (and *trace-level* (>= *trace-level* n))
    (format *trace-output* msg)
    (pprint x *trace-output*))
  (values))
