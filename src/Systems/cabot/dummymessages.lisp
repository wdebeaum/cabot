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
    '(request &key :content (what-next . *))
    #'(lambda (msg args)
	(let ((active-goal (find-arg args :active-goal))
	      (context (remove-minus (find-arg args :context))))
	  (format t "~%~%args = ~S; active-goal = ~S" args active-goal)
	  (process-reply msg args 
			 #||(case active-goal
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
			     ;;  just to give some reply - we didn't anticipate the call||#
			 (case user::*test-dialog-id*
			   ((user::test-generic user::test-not-enough-blocks-and-you-do-it user::test-I-cannot-do-it user::flow-action-topgoal-no user::test-set-system-goal)
			    (let ((action (find-lf-in-context context active-goal)))
			      (case (find-arg action :instance-of)
				(ont::create
				 (case *replyCounter*
				   (0
				    (setq *replyCounter* (+ *replyCounter* 1))
;				    `(REPLY :content (PROPOSE :content (ADOPT :what A5 :as (SUBGOAL :of ,active-goal)) )
;					    :context ((ONT::RELN A5 :instance-of ONT::put-B6-on-the-table)
;						   )
				    `(PROPOSE :content (ADOPT :what A5 :as (SUBGOAL :of ,active-goal)) 
					    :context ((ONT::RELN A5 :instance-of ONT::put-B6-on-the-table)
						   )
					    )
				    )
			   
				   (1
				    (setq *replyCounter* (+ *replyCounter* 1))
				    `(PROPOSE :content (ADOPT :what A6 :as (SUBGOAL :of ,active-goal))
					    :context ((ONT::RELN A6 :instance-of ONT::Please-put-B7-on-B6)
						      (other LFs)
						      )
					    )
				    )
				   (otherwise
				    `(REPORT :content (EXECUTION-STATUS :goal ,active-goal :STATUS ONT::DONE)
					    :context ((we are done)
						      )
					    )
				    )
				   )
				 )
				(ONT::PUT-B6-ON-THE-TABLE
				 `(REPORT :content (EXECUTION-STATUS :goal ,active-goal :STATUS ONT::DONE)
					 :context ((put b6)
						   )
					 ))			     
				(ONT::Please-put-B7-on-B6
				 `(REPORT :content (EXECUTION-STATUS :goal ,active-goal :STATUS ONT::DONE)
					 :context ((put b7)
						   )
					 ))			     
				(ONT::PUT  ; this is not for the test script.  It is here to test generic user commands of "Put ..."
				 `(REPORT :content (EXECUTION-STATUS :goal ,active-goal :STATUS ONT::DONE)
					 :context ((put something)
						   )
					 ))
				#|
				(ONT::BUILD-STAIRCASE
				 `(REPORT :content (EXECUTION-STATUS :goal ,active-goal :STATUS ONT::DONE)
					 :context ((build staircase)
						   )
					 ))			     
				|#
				(otherwise
				 `(REPORT :content (WAIT)))
				))

			    )
			   (user::test-who-move
			    (let ((action (find-lf-in-context context active-goal)))
			      (case (find-arg action :instance-of)
				(ont::create
				 (case *replyCounter*
				   (0
				    (setq *replyCounter* (+ *replyCounter* 1))
				    `(PROPOSE :content (ASK-WH :what A0 :query A1 :as (QUERY-IN-CONTEXT :goal ,active-goal))
					    :context ((ONT::RELN A1 :instance-of ONT::CAUSE-MOVE :AGENT A0 :AFFECTED A2)
						      (ONT::THE A0 :instance-of ONT::PERSON :SUCHTHAT A1)
						      (ONT::THE A2 :INSTANCE-OF ONT::SET :ELEMENT-TYPE ONT::BLOCK)
						   )
					    )
				    )

				   (1
				    (setq *replyCounter* (+ *replyCounter* 1))
				    `(PROPOSE :content (ADOPT :what A5 :as (SUBGOAL :of ,active-goal))
					    :context ((ONT::RELN A5 :instance-of ONT::put-B6-on-the-table)
						   )
					    )
				    )
			   
				   (2
				    (setq *replyCounter* (+ *replyCounter* 1))
				    `(PROPOSE :content (ADOPT :what A6 :as (SUBGOAL :of ,active-goal))
					    :context ((ONT::RELN A6 :instance-of ONT::Please-put-B7-on-B6)
						      (other LFs)
						      )
					    )
				    )
				   (otherwise
				    `(REPORT :content (EXECUTION-STATUS :goal ,active-goal :STATUS ONT::DONE)
					    :context ((we are done)
						      )
					    )
				    )
				   )
				 )
				(ONT::PUT-B6-ON-THE-TABLE
				 `(REPORT :content (EXECUTION-STATUS :goal ,active-goal :STATUS ONT::DONE)
					 :context ((put b6)
						   )
					 ))			     
				(ONT::Please-put-B7-on-B6
				 `(REPORT :content (EXECUTION-STATUS :goal ,active-goal :STATUS ONT::DONE)
					 :context ((put b7)
						   )
					 ))			     
				(ONT::PUT  ; this is not for the test script.  It is here to test generic user commands of "Put ..."
				 `(REPORT :content (EXECUTION-STATUS :goal ,active-goal :STATUS ONT::DONE)
					 :context ((put something)
						   )
					 ))			     
				(otherwise
				 `(REPORT :content (WAIT)))
				))

			    )

			   ((user::flow-staircase-user-directed user::flow-action-topgoal-yes)
			    (let ((action (find-lf-in-context context active-goal)))
			      (case (find-arg action :instance-of)
				(ont::CREATE
				 `(REPORT :content (EXECUTION-STATUS :goal ,active-goal :STATUS ONT::WAITING-FOR-USER)
					 :context ((waiting for user)
						   )
					 ))			     
				(ont::PUT
				 `(REPORT :content (EXECUTION-STATUS :goal ,active-goal :STATUS ONT::DONE)
					 :context ((done putting)
						   )
					 ))			     
				(otherwise 
				  `(REPORT :content (WAIT))
				  )
				)))
			   
			   
			   (user::alarm-test
			    (let ((action (find-lf-in-context context active-goal)))
			      (case (find-arg action :instance-of)
				(ont::create
				 `(PROPOSE :content (ADOPT :what A5 :as (SUBGOAL :of ,active-goal))
					 :context ((ONT::RELN A5 :instance-of ONT::Please-put-B6-on-the-table)
						   )
					 ))
				(otherwise 
				  `(REPORT :content (EXECUTION-STATUS :goal A5 :status ont::waiting-for-user))
				))))
			   
			   (otherwise  ;; old stuff; not updated
			    (let ((action (find-lf-in-context context active-goal)))
			      (case (find-arg action :instance-of)
				(ont::startoff-begin-commence-start
				 `(REPLY :content (PROPOSE :content  (ADOPT :what A2 :as (SUBGOAL :of ,active-goal)))
					 :context ((ONT::RELN A2 :instance-of dummy1)
						   (other LFS for PUT a green block on the table)
						   )))
				#||(ont::put
				`(REPLY :content (EXECUTION-STATUS :action ,active-goal :status ONT::DONE)
				))||#
			     (dummy1
			       `(REPLY :content (PROPOSE :content (ADOPT :what A3 :as (SUBGOAL :of ,active-goal)))
				:context ((ONT::RELN A3 :instance-of dummy2)
					  (other LFS for PUT another one next to it))
					  ))
			     (dummy2
			      `(REPLY :content (PROPOSE :content (ADOPT :what A4 :as (SUBGOAL :of ,active-goal)))
				:context ((ONT::RELN A4 :instance-of dummy2)
					  (other LFS for now put a red one next to that one)
				)))

			     (ont::create
			       `(REPLY :content (PROPOSE :content (ADOPT :what A5 :as (SUBGOAL :of ,active-goal)))
				:context ((ONT::RELN A5 :instance-of ONT::Please-put-B6-on-the-table)
					  )
					  ))
			     (ONT::PLEASE-PUT-B6-ON-THE-TABLE
			       `(REPLY :content (PROPOSE :content (ADOPT :what A6 :as (SUBGOAL :of ,active-goal)))
				:context ((ONT::RELN A6 :instance-of ONT::Please-put-B7-on-B6)
					  )
					  ))
			     (ONT::PUT
			      `(REPLY :content (EXECUTION-STATUS :goal ,active-goal :STATUS ONT::DONE)
				:context ((put something)
					  )
					  ))
			     
			     (otherwise
			      `(REPLY :content (WAIT)))
			     ))
			 )
			   
			 ))))
			 
  :subscribe t)

(defun remove-minus (x)
  (if (not (eq x '-))  x))

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


;;=================
;;  BA interaction: eval/commit cycle -- this one rule picks up any evaluate and sends back an acceptance
;;=================

(defcomponent-handler
  '(request &key :content (evaluate  . *))
     #'(lambda (msg args)
	 (process-evaluate msg args))

  :subscribe t)


(defcomponent-handler
  '(tell &key :content (start-conversation . *))
     #'(lambda (msg args)
	 (declare (ignore msg))
	 (restart-dummy)
      )
    :subscribe t)
