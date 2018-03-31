;;;;
;;;; messages.lisp for DUMMY
;;;;
;;;;

(in-package :dummy)

(in-component :dummy)

;;  Here we pick up messages for all modules that don't exist yet, to
;;    allow developers to run the system as they add functionality
;;   As functionality is added, messages should be commented out


;;=================
;;  BA interaction: eval/commit cycle -- this one rule picks up any evaluate and sends back an acceptance
;;=================

(defcomponent-handler
  '(request &key :content (evaluate  . *))
     #'(lambda (msg args)
	 (process-evaluate msg args))

  :subscribe t)

(defcomponent-handler
  '(request &key :content (what-next  . *))
     #'(lambda (msg args)
	 (what-next msg args))

  :subscribe t)
