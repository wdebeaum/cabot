;;;;
;;;; messages.lisp for DUMMY CSM
;;;;
;;;;

(in-package :dummy)

(in-component :dummy)

;;  Here we pick up messages for the CSM module

(defcomponent-handler
  '(request &key :content (interpret-speech-act  . *))
     #'(lambda (msg args)
	 (process-reply msg args (interpret-sa (find-arg args :content))))

  :subscribe t)


(defun interpret-sa (act)
  (let* ((sa (car act))
	 (what (find-arg-in-act act :content))
	 (context (find-arg-in-act act :context))
	 (what-lf (find-lf-in-context context what))
	 (active-goal (find-arg-in-act act :active-goal)))
    (if (eq active-goal '-)  (setq active-goal nil))
  ;;(format t "QUERY CPS on ~S" sa)
  (case sa
    ((propose ont::propose)
     (list 'REPORT :content (list 'ADOPT :what what :as (if (or (null active-goal) (eq active-goal '-))
							    '(GOAL)
;							    (list 'SUBGOAL :of (find-arg-in-act active-goal :what))))
							    (list 'SUBGOAL :of active-goal)))
	   :context context))
    
    (ont::ask-what-is
     
     (let ((id (gentemp "I" om::*ont-package*))
	   (type (find-arg what-lf :instance-of)))
       (cond ((eq type 'ONT::MEDICATION)
;	      (list 'REPORT :content `(ADOPT :what ,id :as (SUBGOAL :OF ,(find-arg-in-act active-goal :what)))
	      (list 'REPORT :content `(ADOPT :what ,id :as (SUBGOAL :OF ,active-goal))
		    :context 
		    (cons `(ont::RELN ,id :instance-of ONT::IDENTIFY :neutral ,what)
			  context)))
	     (T
	      
;	      (list 'REPORT :content `(ADOPT :what ,id :as (SUBGOAL :of ,(find-arg-in-act active-goal :what)))
	      (list 'REPORT :content `(ADOPT :what ,id :as (SUBGOAL :of ,active-goal))
	      
		    :context (cons `(ont::RELN ,id :instance-of ONT::IDENTIFY :neutral ,what)
					    context))))
       ))
	  
    (ont::evaluate-result
     (let ((id (gentemp "I" om::*ont-package*))
	   (id1 (gentemp "I" om::*ont-package*))
	   (test (find-arg-in-act act :test))
	   (type (find-arg what-lf :instance-of)))
;       (list 'REPORT :content `(ADOPT :what ,id :as (SUBGOAL :of ,(find-arg-in-act active-goal :what)))
       (list 'REPORT :content `(ADOPT :what ,id :as (SUBGOAL :of ,active-goal))
	      
	     :context (list* `(ont::RELN ,id :instance-of ONT::EVALUATE :content ,id1)
			     `(ont::reln ,id1 :instance-of ONT::CAUSE-EFFECT :action ,test :result ,what)
					    context))
       ))

    (assertion
     (multiple-value-bind 
	   (newwhat newcontext)
	 (find-events-in-context context)
       (list 'REPORT :content
;	     `(ASSERTION :what ,newwhat :as (CONTRIBUTES-TO :goal ,(find-arg-in-act active-goal :what)))
	     `(ASSERTION :what ,newwhat :as (CONTRIBUTES-TO :goal ,active-goal))
	     
	     :context newcontext
	       )))
    )))

(defcomponent-handler
  '(request &key :content (take-initiative? . *))
     #'(lambda (msg args)
	 (process-reply msg args (apply #'take-initiative? args)))

  :subscribe t)

(defun take-initiative? (&key result goal context)
  (let* (;(goal-id (find-arg-in-act goal :what))
	 (goal-id goal)
	 (goal-lf (find-lf-in-context context goal-id))
	 (goal-type (find-arg goal-lf :instance-of)))
    ;;(format t "~%Checking intitiative: current goal is ~S, goal type is ~S" goal goal-type)
    ;; currently we take initiative based only on the type of goal
      (cond  
	((member goal-id '(ont::V32008 ))
	 `(TAKE-INITIATIVE :result NO :goal ,goal :context ,context))
	((member goal-type '(ONT::IDENTIFY ONT::EVALUATE))
	 `(TAKE-INITIATIVE :result YES :goal ,goal :context ,context))
	(t 
	  `(TAKE-INITIATIVE :result NO :goal ,goal :context ,context)))))
  

(defun find-events-in-context (context)
  "This returns the ids of all events in the context"
  (let ((id (gentemp "I" om::*ont-package*))
	(event-ids (mapcar #'cadr (remove-if-not #'(lambda (x) (and (member (car x) '(ont::RELN RELN))
								    (member (fourth x) '(ont::ACTIVATE ACTIVATE ONT::BIND)))) context))))
    (format t "~%EVENTS extracted are ~S" event-ids)
    (values id 
	    (cons `(ONT::RELN ,id :instance-of ONT::EVENTS-IN-MODEL :events ,event-ids)
		  context))))
