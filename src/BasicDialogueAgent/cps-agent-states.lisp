;;  Basic Dialogue Agent State Management

(in-package :dagent)

;;(defvar *state-definitions* nil)

(add-state 'propose-cps-act
 (state :action '(SAY-ONE-OF  :content ("What do you want to do?"))
	:transitions (list
		      #|
		      (transition
		       :description "Forget it.  Let's start over."
		       :pattern '((ONT::SPEECHACT ?!sa (? x ONT::PROPOSE ONT::REQUEST) :what ?!what)
				  (?spec ?!what (? t ONT::FORGET ONT::CANCEL ONT::RESTART))
				  (ont::eval (generate-AKRL-context :what ?!what :result ?akrl-context))
				  (ont::eval (find-attr :result ?goal :feature ACTIVE-GOAL))
				  -propose-restart>
				  (RECORD  CPS-HYPOTHESIS (PROPOSE :content ?!what :context ?akrl-context :active-goal ?goal))
				  (INVOKE-BA :msg (INTERPRET-SPEECH-ACT :content (PROPOSE :content ?!what :context ?akrl-context
											   :active-goal ?goal))))
		       :destination 'handle-csm-response
		       :trigger t)
		      |#

		      (transition
		       :description "Let's build a model/I need to find a treatment for cancer"
		       :pattern '((ONT::SPEECHACT ?!sa (? x ONT::PROPOSE ONT::REQUEST) :what ?!what)
				  ((? spec ONT::EVENT ONT::EPI) ?!what ?!t)
				  (ont::eval (generate-AKRL-context :what ?!what :result ?akrl-context))
				  (ont::eval (find-attr :result ?goal :feature ACTIVE-GOAL))
				  -propose-goal>
				  (RECORD  CPS-HYPOTHESIS (PROPOSE :content ?!what :context ?akrl-context :active-goal ?goal))
				  (INVOKE-BA :msg (INTERPRET-SPEECH-ACT :content (PROPOSE :content ?!what :context ?akrl-context
											   :active-goal ?goal))))
				  
		       :destination 'handle-csm-response
		       :trigger t)
		      (transition
		       :description "what drug should we use?"
		       :pattern '((ONT::SPEECHACT ?!sa (? s-act ONT::ASK-WHAT-IS ONT::ASK-IF) :what ?!what)
				  (ONT::TERM ?!what ?object-type)
				  (ont::eval (generate-AKRL-context :what ?!what :result ?akrl-context))  
				  (ont::eval (find-attr :result ?goal :feature ACTIVE-GOAL))
				  -propose-goal-via-question>
				  (RECORD  CPS-HYPOTHESIS (ONT::ASK-WHAT-IS :content ?!what :context ?akrl-context :active-goal ?goal))
				  (INVOKE-BA :msg (INTERPRET-SPEECH-ACT :content (ONT::ASK-WHAT-IS :content ?!what :context ?akrl-context
												   :active-goal ?goal))))
				  
		       :destination 'handle-csm-response
		       :trigger t)

		      ; This should go after the previous (-propose-goal-via-question>)
		      (transition
		       :description "Does the BRAF-NRAS complex vanish?"
		       :pattern '((ONT::SPEECHACT ?!sa (? s-act ONT::ASK-IF) :what ?!what)
				  (?!spec ?!what ?!type)
				  (ont::eval (generate-AKRL-context :what ?!what :result ?akrl-context))  
				  (ont::eval (find-attr :result ?goal :feature ACTIVE-GOAL))
				  -ask-question>
				  (RECORD  CPS-HYPOTHESIS (ONT::ASK-IF :content ?!what :context ?akrl-context :active-goal ?goal))
				  (INVOKE-BA :msg (INTERPRET-SPEECH-ACT :content (ONT::ASK-IF :content ?!what :context ?akrl-context
												   :active-goal ?goal))))
		       :destination 'handle-csm-response
		       :trigger t)
		      
		      (transition
		       :description "is ERK activated if we add X"
		       :pattern '((ONT::SPEECHACT ?!sa ONT::ASK-CONDITIONAL-IF :what ?!what :condition ?!test)
				  (ONT::EVENT ?!what ONT::SITUATION-ROOT)
				  (ONT::EVENT ?!test ONT::EVENT-OF-CAUSATION) 
				  (ont::eval (generate-AKRL-context :what ?!what :result ?akrl-context))
				  (ont::eval (find-attr :result ?goal :feature ACTIVE-GOAL))
				  -propose-test>
				  (RECORD CPS-HYPOTHESIS (ONT::EVALUATE-RESULT  :content ?!what :test ?!test :context ?akrl-context :active-goal ?goal))
				  (INVOKE-BA :msg (INTERPRET-SPEECH-ACT :content (ONT::EVALUATE-RESULT :content ?!what :test ?!test :context ?akrl-context
						   :active-goal ?goal))
				  ))
		       :destination 'handle-csm-response
		       :trigger t)
		      (transition
		       :description "Kras activates Raf -- as performing steps in elaborating a model"
		       :pattern '((ONT::SPEECHACT ?!sa ONT::TELL :what ?!root)  ;; we allow intermediate verbs between SA and activate (e.g., KNOW)
				  ;;(ONT::EVENT ?!what ONT::ACTIVATE :agent ?!agent :affected ?!affected)
				  (ont::eval (generate-AKRL-context :what ?!root :result ?akrl-context))
				  (ont::eval (find-attr :result ?goal :feature ACTIVE-GOAL))
				  -refine-goal-with-assertion>
				  (RECORD  CPS-HYPOTHESIS (ASSERTION :content ?!root :context ?akrl-context :active-goal ?goal))
				  (INVOKE-BA :msg  (INTERPRET-SPEECH-ACT :content (ASSERTION :content ?!root :context ?akrl-context
						    :active-goal ?goal))
				  ))
				  
		       :destination 'handle-csm-response
		       :trigger t)
		      
		      )
	))

(add-state 'handle-CSM-response
	   (state :action nil
		  :transitions (list
		      (transition
		       :description "CSM returns a successful proposal interpretation"
		       :pattern '((BA-RESPONSE X (? act ADOPT ASSERTION) :what ?!goal :as ?as :context ?new-akrl :alternative ?alt-as)
				  -successful-interp1>
				  (UPDATE-CSM (PROPOSED :content (ADOPT :what ?!goal :as ?as)
					       :context ?new-akrl))
				  (RECORD PROPOSAL-ON-TABLE (ONT::PROPOSE-GOAL :Content (ADOPT :what ?!goal :as ?as)
							     :context ?new-akrl))
				  (RECORD ACTIVE-GOAL ?!goal)
				  (RECORD ALT-AS ?alt-as)
				  (INVOKE-BA :msg (EVALUATE 
						   :content ((? act ADOPT ASSERTION) :what ?!goal :as ?as)
						   :context ?new-akrl))
				  )
		       :destination 'propose-cps-act-response
		       )

		      
		      ;; failure: can't identify goal
		      ;;(TELL :RECEIVER DAGENT :CONTENT (REPORT :content (FAILED-TO-INTERPRET :WHAT ONT::V32042 :REASON (MISSING-ACTIVE-GOAL) :POSSIBLE-SOLUTIONS (ONT::BUILD-MODEL)) :context ()) :IN-REPLY-TO IO-32505 :sender CSM)
		      (transition
		       :description "CSM fails to identify the goal, but has a guess"
		       :pattern '((BA-RESPONSE  X FAILURE :type FAILED-TO-INTERPRET :WHAT ?content :REASON (MISSING-ACTIVE-GOAL) 
				   :POSSIBLE-RESOLUTION (?!possible-goal) :context ?context)
				  (ont::eval  (extract-goal-description :cps-act ?!possible-goal :context ?context :result ?goal-description :goal-id ?goal-id))
			     				  
				  -intention-failure-with-guess>
				  (RECORD FAILURE (FAILED-TO-INTERPRET :WHAT ?!possible-goal :REASON (MISSING-ACTIVE-GOAL) :POSSIBLE-SOLUTIONS (?!possible-goal) :context ?context))
				  (RECORD POSSIBLE-GOAL ?!possible-goal)
				  (RECORD POSSIBLE-GOAL-ID ?goal-id)
				  (RECORD POSSIBLE-GOAL-CONTEXT ?goal-description))
		       :destination 'clarify-goal
		       )
		      
		      (transition
		       :description "CSM fails to identify the goal, and no guess. Right now we just prompt the user
                                     to identify their goal and forget the current utterance"
		       :pattern '((BA-RESPONSE  X FAILURE :type FAILED-TO-INTERPRET :WHAT ?content :REASON (MISSING-ACTIVE-GOAL) :context ?context)
				  -intention-complete-failure>
				  (RECORD FAILURE (FAILED-TO-INTERPRET :WHAT ?content :REASON (MISSING-ACTIVE-GOAL) :context ?context))
				  (RECORD POSSIBLE-GOAL nil)
				  )
		       :destination 'propose-cps-act
		       )
		      
		  )))
	  
		      
(add-state 'propose-cps-act-response
	   (state :action nil
		  :Transitions (list
				(transition
				 :description "acceptance"
				 :pattern '((BA-RESPONSE X ACCEPTABLE :what ?!psgoal :context ?!context)
					    (ont::eval (extract-feature-from-act :result ?goal-id :expr ?!psgoal :feature :what))
					    -goal-response1>
					    (UPDATE-CSM (ACCEPTED :content ?!psgoal :context ?!context))
					    
					    (notify-BA :msg (COMMIT
							     :content ?!psgoal)) ;; :context ?!context))  SIFT doesn't want the context
					    (RECORD ACTIVE-GOAL ?goal-id)
					    (RECORD ACTIVE-CONTEXT ?!context)
					    (generate :content (ONT::ACCEPT)))
				 
				 :destination 'what-next-initiative)
				(transition
				 :description "rejectance"
				 :pattern '((BA-RESPONSE (? x REJECT UNACCEPTABLE) ?!psobj :context ?!context )
					    ;;(ont::eval (find-attr ?goal  GOAL))
					    -goal-response2>
					    (RECORD REJECTED ?!psobj :context ?context))
				 :destination 'explore-alt-interp)
				)
		  ))

(add-state 'explore-alt-interp
	   (state :action nil
		  :Transitions (list
				(transition
				 :description "we have a backup interpretation"
				 :pattern '((ont::eval (find-attr :result ?!alt :feature ALT-AS))
					    -alt-found1>
					    (RECORD PROPOSAL-ON-TABLE (ONT::PROPOSE-GOAL :Content (ADOPT :what ?!goal :as ?!alt)
								       ))
					    (RECORD ALT-AS nil)
					    (INVOKE-BA :msg (EVALUATE 
							     :content (ADOPT :what ?!goal :as ?altas)
							     :context ?new-akrl)))
				 
				 :destination 'propose-cps-act-response)
				
				(transition
				 :description "no backup alt left"
				 :pattern '((ont::eval (find-attr :result nil :feature ALT-AS))
					    -no-alt>
					    (say :content "I didn't understand what you are trying to do")
					    ))
				)))

;;  CLARIFICATION MANAGEMENT

(add-state 'clarify-goal 
	   (state :action '(GENERATE :content (ONT::CLARIFY-GOAL :content (V possible-goal-id) :context (V POSSIBLE-GOAL-context))
			  )
		  :preprocessing-ids '(yes-no)
		  :transitions
		  (list
		   (transition
		    :description "yes"
		    :pattern '((ANSWER :value YES)
			       (ont::eval (find-attr :result ?context :feature POSSIBLE-GOAL-context))
			       (ont::eval (find-attr :result ?poss-goal :feature possible-goal))
			       -right-guess-on-goal>
			       (INVOKE-BA :msg (EVALUATE 
						:content ?poss-goal
						:context ?context))
			       
			       )
		    :destination 'confirm-goal-with-BA)
		   
		   (transition
		    :description "no"
		    :pattern '((ANSWER :value NO)
			       -propose-cps-act>
			       (SAY :content "OK"))
		    :destination 'propose-cps-act
		    )
		  )))

(add-state 'confirm-goal-with-BA
	   (state :action nil
		  :transitions (list
				(transition
				 :description "check with BA that the clarified goal is acceptable"
				 :pattern '((BA-RESPONSE X ACCEPTABLE :what ?!psgoal :context ?!context)
					    (ont::eval (extract-feature-from-act :result ?goal-id :expr ?!psgoal :feature :what))
					    (ont::eval (find-attr :result ?orig-cps-hyp :feature CPS-HYPOTHESIS))
					    -confirmed-clarify-goal>
					    (UPDATE-CSM (ACCEPTED :content ?!psgoal :context ?!context))
					    
					    (notify-BA :msg (COMMIT
							     :content ?!psgoal)) ;; :context ?!context))  SIFT doesn't want the context
					    (RECORD ACTIVE-GOAL ?goal-id)
					    (RECORD ACTIVE-CONTEXT ?!context)
					    (SAY :content "Great!")
					    ;;  Now we try to reinterpret the original utterance that caused the clarification
					    (INVOKE-BA :msg (INTERPRET-SPEECH-ACT :content ?orig-cps-hyp :active-goal ?goal-id)))
				 :destination 'handle-CSM-response
				))))



;;   INTITIATIVE MANAGEMENT
;; This state starts an interaction with the BA to determine if the system should take
;;  initiative or not

(add-state 'what-next-initiative
	   (state :action '(take-initiative? :goal (V active-goal) :context (V active-context))
		  :transitions (list
				(transition
				 :description "decided on taking initiative"
				 :pattern '((TAKE-INITIATIVE :result YES :goal ?!result :context ?context)
					    -take-init1>
					    (UPDATE-CSM (INITIATIVE-TAKEN-ON-GOAL :what ?!result :context ?context))
					    (invoke-BA :msg (WHAT-NEXT :active-goal ?!result :context ?context))
					    )
				 :destination 'perform-BA-request)
				;;; initiative declined, enter a wait state
				(transition
				 :description "no initiative"
				 :pattern '((TAKE-INITIATIVE :result NO)
					    -take-init2>
					    (UPDATE-CSM (NO-INITIATIVE-TAKEN)))
				 :destination 'segmentend)
				)
		  ))

(add-state 'perform-BA-request
	   (state :action nil
		  :transitions (list
				(transition
				 :description "failed trying to achieve the goal"
				 :pattern '((BA-RESPONSE X FAILURE :what ?!F1 :as (SUBGOAL :of ?!target) :context ?context)
					    ;;(A F1 :instance-of ONT::LOOK-UP :neutral ?!target)
					    -failed1>
					    (UPDATE-CSM (FAILED-ON  :what ?!F1 :as (SUBGOAL :of ?!target) :context ?context))
					    (GENERATE 
					     :content (ONT::TELL :content (ONT::FAIL :formal ?!F1 :tense PAST))
					     :context ?context
					     )
					    )
				 :destination 'segmentend)
				(transition
				 :description "solution to goal reported"
				 :pattern '((BA-RESPONSE X SOLUTION :what ?!what :goal ?goal :context ?akrl-context)
					    ;;(ont::eval (generate-AKRL-context :what ?!what :result ?akrl-context))
					    -soln1>
					    (UPDATE-CSM (SOLVED :what ?!what :goal ?goal :context ?akrl-context))
					    (GENERATE 
					     :content (ONT::TELL :content ?!what)
					     :context ?akrl-context
					     )
					    )
				 :destination 'segmentend)
				
				;;; initiative declined, enter a wait state
				(transition
				 :description "BA has nothing to do"
				 :pattern '((BA_RESPONSE ?!x WAIT)
					    -wait>
					    (UPDATE-CSM (BA-WAITING)))
				 :destination 'segmentend)
				
				(transition
				 :description "suggestion of user action"
				 :pattern '((BA-RESPONSE ?!X PERFORM :agent *USER* :action ?!action :context ?context)
					    -what-next1>
					    (UPDATE-CSM (PROPOSED :content ?!action :context ?context))
					    (RECORD PROPOSAL-ON-TABLE (ONT::PROPOSE :content ?!action :context ?context))
					    (GENERATE :content (ONT::PROPOSE :content (ONT::PERFORM :action ?!action :context ?context))))
				 :destination 'proposal-response)
				(transition
				 :description "action completed!"
				 :pattern '((BA-RESPONSE ?!X GOAL-ACHIEVED)
					    -what-next3>
					    (UPDATE-CSM (GOAL-ACHIEVED))
					    (GENERATE :content (ONT::EVALUATION :content (ONT::GOOD)))
					   
					    (GENERATE :content (ONT::CLOSE)))
				 :destination 'segment-end)
				
				(transition
				 :description "BA has nothing to do"
				 :pattern '((BA_RESPONSE ?!x WAIT)
					    -what-next4>
					    (UPDATE-CSM (BA-WAITING)))
				 :destination 'segment-end)
				)

		  ))

;;;;
;;    Here are the acts starting a dialogue with system intitative


;;  Setting CPS GOALS  -- System Initiative
;;  This assume that the CPS state has a preset proviate system goal that
;; we want to make into a shared goal with the user
;; N ote:: this assumes the private system goal is already cached for processing
(add-state 'initiate-CPS-goal
	   (state :action nil
		  :transitions
		  (list (transition
			 :description "we know the private goal, so we propose it to the user"
			 :pattern '((CSM-RESPONSE ?!x PRIVATE-SYSTEM-GOAL :content ?!content :context ?context)
				    -propose-goal>
				    (RECORD PROPOSAL-ON-TABLE (ONT::PROPOSE-GOAL :Content ?!content :context ?context))
				    (GENERATE :content (ONT::PROPOSE-GOAL :content ?!content :context ?context)))
			 :destination 'initiate-csm-goal-response))))

(add-state 'initiate-csm-goal-response
	   (state :action nil
		  :transitions (list
				
				;; If the user rejects this, we ask them to propose something
				(transition
				 :description "rejectance"
				 :pattern '((ONT::SPEECHACT ?sa1 ONT::REJECT )
					    -intitiate-response2>
					    (UPDATE-CSM (REJECTED :content ?!content :context ?!context))
					    (GENERATE :content (ONT::REQUEST :content (ONT::PROPOSE-GOAL :agent *USER*))))
				 :destination 'segment-end)
				(transition
				 :description "acceptance"
				 :pattern '((ONT::SPEECHACT ?!sa ONT::ACCEPT)
					    (ont::eval (find-attr :result (?prop :content ?!content :context ?!context) 
							:feature PROPOSAL-ON-TABLE))
					    -initiate-response1>
					    (UPDATE-CSM (ACCEPTED (?prop :content ?!content :context ?!context)))
					    (RECORD ACTIVE-GOAL ?!content)
					    (NOTIFY-BA :msg (SET-SHARED-GOAL :content ?!content
							     :context ?!context)))
				 
				 :destination 'what-next-initiative)
				)
		  ))

(add-state 'user-prompt
	    (state :action nil
		  :transitions (list
				(transition
				 :description "what next"
				 :pattern '((ONT::WH-TERM ?!sa ONT::REFERENTIAL-SEM :proform ont::WHAT)
					    ((? sp ONT::F ONT::EVENT) ?s1 ONT::SEQUENCE-VAL)
					    -what-next>
					    (invoke-BA :msg (WHAT-NEXT :active-goal ?!result :context ?context)))
				 :destination 'perform-BA-request
				 :trigger t)
				;; If the user rejects this, we ask them to propose something
				(transition
				 :description "what should we do next?"
				 :pattern '((ONT::SPEECHACT ?sa1 ONT::S_WH-QUESTION :focus ?!foc)
					    (ONT::WH-TERM ?!sa ONT::REFERENTIAL-SEM :proform ont::WHAT)
					    
					    -what-next2>
					    (invoke-BA :msg (WHAT-NEXT :active-goal ?!result :context ?context)))
				 :destination 'perform-BA-request)
				)
		  ))

				
(add-state 'proposal-response
	   (state :action nil
		  :transitions (list
				(transition
				 :description "OK/ accept"
				 :pattern '((ONT::SPEECHACT ?!sa ONT::ACCEPT)
					    (ont::eval (find-attr :result (?prop :content ?!content :context ?!context)  
							:feature PROPOSAL-ON-TABLE))
					    -proposal-response1>
					    (UPDATE-CSM  (ACCEPTED (?prop :content ?!content :context ?!context)))
					    (NOTIFY-BA :msg (NOTIFY-WHEN-COMPLETED :agent *USER* :content ?!content :context ?!context))
					    )
				 :destination 'expect-action)
				(transition
				 :description "action completed (from BA)"
				 :pattern '((EXECUTION-STATUS :action ?!act :status ont::DONE)
					    -demonstrate-action1>
					    (RECORD ACTIVE-GOAL ?!act)
					    )
				 :destination 'what-next-initiative)
				)
		  ))

;; here we are waiting for something to happen in the world

(add-state 'expect-action
	   (state :action nil
		  :transitions (list
				(transition
				 :description "action completed (from BA)"
				 :pattern '((EXECUTION-STATUS :action ?!act :status ont::DONE)
					    -demonstrate-action2>
					    (RECORD LAST-ACTIOn-DONE ?!act)
					    )
				 :destination 'what-next-initiative)
				;;  user might speak while we're waiting for confirmation
				
				(transition
				 :description "what next"
				 :pattern '((ONT::WH-TERM ?!sa ONT::REFERENTIAL-SEM :proform ont::WHAT)
					    ((? sp ONT::F ONT::EVENT) ?s1 ONT::SEQUENCE-VAL)
					    -what-next>
					    (nop))
				 :destination 'what-next-initiative)
								
				;; OK might have come in after the action was performed
				(transition
				 :description "OK/ accept"
				 :pattern '((ONT::SPEECHACT ?!sa ONT::ACCEPT)
					    -OK-confirm-done>
					    (NOP)
					    )
				  :destination 'what-next-initiative)
								
				(transition
				 :description "ONT::OK from BA"
				 :pattern '((ONT::OK)
					    -confirm>
					    (NOP))
				 :destination 'expect-action)
				)
		  ))


