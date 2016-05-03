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
;;  BA interactions
;;=================

(defcomponent-handler
    '(request &key :content (SET-SHARED-GOAL . *))
    #'(lambda (msg args)
	(process-reply  msg args
				  '(ONT::OK)))
  :subscribe t)

(defcomponent-handler
    '(request &key :content (what-next . *))
    #'(lambda (msg args)
	(let ((active-goal (find-arg args :active-goal)))
	  (format t "~%~%args = ~S; active-goal = ~S" args active-goal)
	  (process-reply msg args 
			 (case active-goal
			     (G1 ;; ont::BUILD
			      #||'(REPLY :content (ONT::PERFORM :agent *USER* :action A1)
				:context
				((A A1 :instance-of ONT::PUT :agent *USER* :affected b1 :result on1)
				 (RELN on1 :instance-of ont::ON :of b1 :val t1)
				 (A b1 :instance-of ont::BLOCK)
				 (THE t1 :instance-of ont::TABLE))))
			      '(REPLY :content (ONT::PERFORM :agent *USER* :action A1)
				:context ((ISA (SKOLEM-FN PLAN-BLK0) BLOCK)
					  (ON (SKOLEM-FN PLAN-BLK0) GRD))))||#
			      '(REPLY :CONTENT (PERFORM :AGENT *USER* 
						:ACTION ACHIEVE) :CONTEXT ((ISA (SKOLEM-FN PLAN-BLK0) BLOCK) (ON 
													      (SKOLEM-FN PLAN-BLK0) GRD))))
			      ;;  there are now two blocks on the table, say b1 and b2
			     ;;   this is something like "move b1 until it touches b2" (a simpler version of "push them together")
			     (A1
			      '(REPLY :content (PERFORM :agent *USER* :action A2)
				:context ((A A2 :instance-of ONT::PUSH :agent *USER* :affected b1 :result to1)
					  (RELN to1 :instance-of ont::TOUCH :neutral b1 :neutral1 b2)
					  )))
			     (A2
 			      '(REPLY :content (PERFORM :agent *USER* :action A3)
				:context ((A A3 :instance-of ONT::MAKE-IT-SO :agent *USER*  :result on2)
					   (RELN on2 :instance-of ont::ON :of b3 :val b2)
					   (A b3 :instance-of ONT::BLOCK))
					  ))
			     (A3
			      '(REPLY :content (GOAL-ACHIEVED)))
			     ;;  just to give some reply - we didn't anticipate the call
			     (otherwise
			      '(ONT::ACT))
			     ))))
			 
  :subscribe t)

(defcomponent-handler
  '(request &key :content (accepted . *))
     #'(lambda (msg args)
	 (process-reply msg args
				 '(ONT::OK)))

  :subscribe t)

#||(defcomponent-handler
  '(request &key :content (notify-completed . *))
     #'(lambda (msg args)
	 (process-reply msg args
				 '(ONT::OK)))
  :subscribe t)||#

(defcomponent-handler
    '(tell &key :content (scene-description . *))
    #'(lambda (msg args)
	(let ((time (find-arg args :timestamp))
	      )
	  (process-reply msg args
			 (cond ((equal time "2015-08-17T01:45:09.4123")
				'(EXECUTION-STATUS :action A1 :status ont::INCOMPLETE))
			       ((equal time "2015-08-17T01:45:23.1361")
				'(EXECUTION-STATUS :action A1 :status ont::INCOMPLETE))
			       
			       ((equal time "2015-08-17T01:45:33.1353")
				'(EXECUTION-STATUS :action A1 :status ont::DONE))

			       ((equal time "2015-08-17T01:45:44.2145")
				'(EXECUTION-STATUS :action A2 :status ont::DONE))

			       ((equal time "2015-08-17T01:45:58.0941")
				'(EXECUTION-STATUS :action A3 :status ont::DONE))
				))))
  
  
  :subscribe t)
