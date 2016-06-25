;;;;
;;;; messages.lisp for DUMMY
;;;;
;;;;

(in-package :dummy)

(in-component :dummy)

;;;===========

;;;    Here we have the dummy code to simulate the CSM

(defcomponent-handler
    '(request &key :content (UPDATE-CSM . *))
    #'(lambda (msg args)
	(update-csm (find-arg args :content)))
  :subscribe t)

(defcomponent-handler
    '(request &key :content (QUERY-CSM . *))
    #'(lambda (msg args)
	(process-reply msg args 
		       (query-csm (find-arg args :content))))
  :subscribe t)

(defvar *CPS-state* nil)
(defvar *proposed-CPS-state* nil)
(defvar *current-CPS-state* nil)

(defun set-CPS-variable (var value)
  (push (list var value) *CPS-state*)
  (format t "~%*CPS-STATE* now is ~S"  *CPS-state*))

(defun lookupCPSvar (var)
  (cadr (assoc var *CPS-state*)))

(defun current-goal-id nil
  (let ((c (current-cps-goal)))
    (case (car c)
      ((goal subgoal) 
       (let ((what  (find-arg-in-act c :what)))
	 (if (consp what)
	     (find-arg-in-act what :what)
	     what))))))

(defun current-cps-goal nil
  (car *current-cps-state*))

(defun backup-cps-state nil
  "This is useful when debugging to remove whatever was updated last"
  (setq *current-cps-state* (cdr *current-cps-state*)))

		
(defun find-events-in-context (context)
  "This returns the ids of all events in the context"
  (let ((id (gentemp "I"))
	(event-ids (mapcar #'cadr (remove-if-not #'(lambda (x) (and (member (car x) '(ont::RELN RELN))
								    (member (fourth x) '(ont::ACTIVATE ACTIVATE)))) context))))
    (format t "~%EVENTS extracted are ~S" event-ids)
    (values id 
	    (cons `(ONT::RELN ,id :instance-of ONT::EVENTS-IN-MODEL :events ,event-ids)
		  context))))
	
(defun take-initiative? (&key result)
  (let* ((current-goal (current-cps-goal))
	 (what (current-goal-id))
	 (context (find-arg-in-act current-goal :context))
	 (goal (lookup-lf what context))
	 (goal-act (find-arg goal :instance-of)))
    (format t "~%Checking intitiative: current goal is ~S" goal)
    ;; currently we take initiative based only on the type of goal
      (case goal-act
	((ont::identify identify)
	 ;;(im::match-vals nil result current-goal)
	 (cache-response-for-processing `((TAKE-INITIATIVE :result YES :goal ,what :context ,context))))
	(otherwise
	 (cache-response-for-processing '((TAKE-INITIATIVE :result NO))))
	)
      ))
   

(defun lookup-lf (what context)
  (find what context :key #'cadr))
;; Here DAGENT has received an externally set goal, we update the CPS state
;; and initiate the process of establishing a joint goal with the user


(defun update-csm (update)
  (format t "~%Update request receieved: ~S" update)
  (let* ((content (find-arg-in-act update :content))
	 (as (if (consp content) (find-arg-in-act content :as)))
	 (what (if (consp content) (find-arg-in-act content :what)))
	 (context (find-arg-in-act update :context)))
    (case (car update)
      (proposed
       (setq  *proposed-CPS-state* (list :content content :context context)))
      (accepted
       (if as
	   (push (append as (list :what what :context context)) *current-CPS-state*)
	   (push (list :what content :context context) *current-CPS-state*)))
      (solved
       (let ((goalid (find-arg-in-act what :goal)))
	 (if (eq goalid (current-goal-id))
	     (pop *current-CPS-state*))))
      )))

(defun query-csm (content)
  (case (car content)
    (private-system-goal 
     '(PRIVATE-SYSTEM-GOAL :CONTENT G1 :CONTEXT
       ((A G1 :INSTANCE-OF ONT::BUILD :AFFECTED-RESULT ST1)
	(A ST1 :INSTANCE-OF ONT::STAIRCASE))))
    ))

(defun interpret-sa (&key sa what result context new-akrl-context)
  ;;(format t "QUERY CPS on ~S" sa)
  (case sa
    (propose
     (append (im::match-vals nil result `(ADOPT :what ,what :as ,(if (null (current-cps-goal)) 
								     '(GOAL)
								     (list 'SUBGOAL :of (current-goal-id)))))
	     (im::match-vals nil new-akrl-context context)))
    (last-proposal
     (format t "~%LAST PROPOSAL IS ~S" *proposed-CPS-state*)
     (append (im::match-vals nil result (second *proposed-CPS-state*))
	     (im::match-vals nil context (fourth *proposed-CPS-STATE*))))
    (query
     (let ((id (gentemp "I")))
       (cond ((null (current-cps-goal))
	      (append (im::match-vals nil result `(ADOPT :what ,id :as (GOAL)))
		      (im::match-vals nil new-akrl-context 
				      (cons `(ont::RELN ,id :instance-of ONT::IDENTIFY :affected ,what)
					    context))))
	     ((member (car (current-cps-goal)) '(goal subgoal))
	      
	      (append (im::match-vals nil result 
				      `(ADOPT :what ,id :as (SUBGOAL :of ,(current-goal-id))))
		      (im::match-vals nil new-akrl-context 
				      (cons `(ont::RELN ,id :instance-of ONT::IDENTIFY :affected ,what)
					    context)))))
       ))
	   
    (assertion
     (multiple-value-bind 
	   (newwhat newcontext)
	 (find-events-in-context context)
       (append (im::match-vals nil result 
			       `(ASSERTION :what ,newwhat :as (CONTRIBUTES-TO :goal ,(current-goal-id))))
	       )))
    ))
	     
