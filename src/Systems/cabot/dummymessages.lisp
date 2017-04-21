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
;;  Ba interactions
;;=================

(defcomponent-handler
    '(request &key :content (what-next . *))
    #'(lambda (msg args)
	(let ((active-goal (find-arg args :active-goal))
	      (context (remove-minus (find-arg args :context))))
	  (format t "~%~%args = ~S; active-goal = ~S" args active-goal)
;	  (format t "~%action = ~S; switch for case statements = ~S~%"
;		  (find-lf-in-context context active-goal)
;		  (find-arg (find-lf-in-context context active-goal) :instance-of))
	  (process-reply msg args 
			 (case user::*test-dialog-id*
			   ((user::test-generic user::test-not-enough-blocks-and-you-do-it user::test-I-cannot-do-it user::flow-action-topgoal-no user::test-set-system-goal user::test-instead user::test-i-will-tell-you user::test-staircase-green-block)
			    (let ((action (find-lf-in-context-tmp context active-goal)))
			      (case (find-arg action :instance-of)
				(ont::create
				 (case *replyCounter*
				   (0
				    (setq *replyCounter* (+ *replyCounter* 1))
;				    `(REPLY :content (PROPOSE :content (ADOPT :what A5 :as (SUBGOAL :of ,active-goal)) )
;					    :context ((ONT::RELN A5 :instance-of ONT::put-B6-on-the-table)
;						   )
				    `(PROPOSE :content (ADOPT :id G5 :what A5 :as (SUBGOAL :of ,active-goal)) 
					    :context ((ONT::RELN A5 :instance-of ONT::put-B6-on-the-table)
						   )
					    )
				    )
			   
				   (1
				    (setq *replyCounter* (+ *replyCounter* 1))
				    `(PROPOSE :content (ADOPT :id G6 :what A6 :as (SUBGOAL :of ,active-goal))
					    :context ((ONT::RELN A6 :instance-of ONT::Please-put-B7-on-B6)
						      (other LFs)
						      )
					    )
				    )
				   (2
				    (setq *replyCounter* (+ *replyCounter* 1))
				    (if (eq user::*test-dialog-id* 'user::test-I-cannot-do-it)
					`(PROPOSE :content (ADOPT :id G11 :what A11 :as (SUBGOAL :of ,active-goal))
						  :context ((ONT::RELN A11 :instance-of ONT::you-suggest-something)
							   )
						 )
					`(REPORT :content (EXECUTION-STATUS :goal ,active-goal :STATUS ONT::DONE)
						 :context ((we are done)
							   )
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
				 `(REPORT :content (EXECUTION-STATUS :goal ,active-goal :STATUS ONT::WAITING-FOR-USER)
					 :context ((put b6)
						   )
					 ))			     
				(ONT::Please-put-B7-on-B6
				 `(REPORT :content (EXECUTION-STATUS :goal ,active-goal :STATUS ONT::DONE)
					 :context ((put b7)
						   )
					 ))	
		     
				((ONT::PUT ONT::EXECUTE)
				 `(REPORT :content (EXECUTION-STATUS :goal ,active-goal :STATUS ONT::DONE)
					 :context ((put something)
						   )
					 ))
			
				((nil)  ; note: need parenthesis around nil
				 `(PROPOSE :content (ADOPT :id G12 :what A12 :as (GOAL))
					   :context ((ONT::RELN A12 :instance-of ONT::you-suggest-something)
						     )
					   )
				 )
				
				 
				(otherwise
				 `(REPORT :content (WAIT)))
				))

			    )
			   (user::test-who-move
			    (case *replyCounter*
				   (0
				    (setq *replyCounter* (+ *replyCounter* 1))
				    (setq *last-active-goal* active-goal)
				    `(PROPOSE :content (ASK-WH :id G0 :what A0 :query A1 :as (QUERY-IN-CONTEXT :goal ,active-goal))
					    :context ((ONT::RELN A1 :instance-of ONT::CAUSE-MOVE :AGENT A0 :AFFECTED A2)
						      (ONT::THE A0 :instance-of ONT::PERSON :SUCHTHAT A1)
						      (ONT::THE A2 :INSTANCE-OF ONT::SET :ELEMENT-TYPE ONT::BLOCK)
						   )
					    )
				    )

				   (1
				    (setq *replyCounter* (+ *replyCounter* 1))
				    `(PROPOSE :content (ADOPT :id g2 :what A5 :as (SUBGOAL :of ,active-goal))
					    :context ((ONT::RELN A5 :instance-of ONT::put-B6-on-the-table)
						   )
					    )
				    )
			   
				   (2
				    (setq *replyCounter* (+ *replyCounter* 1))
				    `(PROPOSE :content (ADOPT :id g3 :what A6 :as (SUBGOAL :of ,active-goal))
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
				  
				
				   #||(ONT::PUT-B6-ON-THE-TABLE
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
				    `(REPORT :content (WAIT))))||#
				   ))
			 

			    

			   (user::test-who-move-user
			    (case *replyCounter*
			      (0
			       (setq *replyCounter* (+ *replyCounter* 1))
			       `(REPORT :content (ANSWER :TO ,active-goal ;,*last-query-id*
							 :what ,*last-query-what*
							 :query ,*last-query-id*
							 :VALUE A1 
							 :EFFECT (ADOPT :ID XX1 :WHAT YY1
									:as (MODIFICATION :of ,*last-query-in-context*)))
					:context ((ONT::THE A1 :instance-of ONT::PERSON 
							    :equals user)
						  (ONT::RELN YY1 :instance-of modified-goal)
					 )
			       )
			       )
			      (1
			       (setq *replyCounter* (+ *replyCounter* 1))
			       `(REPORT :content (EXECUTION-STATUS :goal ,active-goal :status ont::waiting-for-user))
			       )
			      (otherwise
			       `(REPORT :content (EXECUTION-STATUS :goal ,active-goal :status ont::done)))
			      ))
			   
			

			   ((user::flow-staircase-user-directed user::flow-action-topgoal-yes)
			    (let ((action (find-lf-in-context-tmp context active-goal)))
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
			   
			   
			   (user::test-alarm-waiting-for-user
			    (let ((action (find-lf-in-context-tmp context active-goal)))
			      (case (find-arg action :instance-of)
				(ont::create
				 `(PROPOSE :content (ADOPT :id G5 :what A5 :as (SUBGOAL :of ,active-goal))
					 :context ((ONT::RELN A5 :instance-of ONT::Please-put-B6-on-the-table)
						   )
					 ))
				(otherwise 
				  `(REPORT :content (EXECUTION-STATUS :goal G5 :status ont::waiting-for-user))
				))))

			   (user::test-alarm-waiting-for-user
			    (let ((action (find-lf-in-context-tmp context active-goal)))
			      (case (find-arg action :instance-of)
				(ont::create
				 `(PROPOSE :content (ADOPT :id G5 :what A5 :as (SUBGOAL :of ,active-goal))
					 :context ((ONT::RELN A5 :instance-of ONT::Please-put-B6-on-the-table)
						   )
					 ))
				(otherwise 
				  `(REPORT :content (EXECUTION-STATUS :goal G5 :status ont::waiting-for-user))
				))))

			   
			   (user::test-multi-goal
			    (let ((action (find-lf-in-context-tmp context active-goal)))
			      (case (find-arg action :instance-of)
				(ont::CREATE
				 `(REPORT :content (ASK-WH :id dd :what ww :query qq)
					 :context ((ONT::RELN qq :instance-of ONT::WORKING :agent we :formal ww)
						   (ONT::THE ww :instance-of ONT::GOAL :suchthat qq)
						   (ONT::A we :instance-of ONT::PERSON :equals ont::us)
						   )
					 ))
				
				(ont::PUT
				 (case *replyCounter*
				   (0
				    (setq *replyCounter* (+ *replyCounter* 1))
				    `(REPORT :content (EXECUTION-STATUS :goal ,active-goal :STATUS ONT::WORKING-ON-IT)
										       :context ((working on it)
												 )
										       ))
				   (otherwise 
				     `(REPORT :content (EXECUTION-STATUS :goal ,active-goal :STATUS ONT::DONE)
										       :context ((done)
												 )
										       ))
				
				   ))
				)))
			   
			   (user::test-user-move-true-ynq
			    (case *replyCounter*
			      (0
			       (setq *replyCounter* (+ *replyCounter* 1))
			       `(REPORT :content (ANSWER :TO ,active-goal ;,*last-query-id*
						      :query ,*last-query-id*
						      :VALUE ONT::TRUE)))
			      (1
			       (setq *replyCounter* (+ *replyCounter* 1))
			       `(PROPOSE :content (ADOPT :id g3 :what A6 :as (SUBGOAL :of ,active-goal))
					    :context ((ONT::RELN A6 :instance-of ONT::Please-put-B7-on-B6)
						      (other LFs)
						      )
					    ))
			      (2 
			        `(REPORT :content (EXECUTION-STATUS :goal ,active-goal :STATUS ONT::WAITING-FOR-USER)
					 :context ((LFS)
						   )
					 ))	
			      
			      ))
						
			   
			   (otherwise  ;; old stuff; not updated
			    `(REPLY :content (update dummymessages))
			    
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
