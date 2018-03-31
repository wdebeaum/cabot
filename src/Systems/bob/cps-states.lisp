;;  Basic Dialogue Agent State Management

(in-package :dagent)

(defvar *state-definitions* nil)

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
		       :pattern '((BA-RESPONSE  X FAILED-TO-INTERPRET :WHAT ?content :REASON (MISSING-ACTIVE-GOAL) :POSSIBLE-SOLUTIONS (?!possible-goal) :context ?context)
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
		       :pattern '((BA-RESPONSE  X FAILED-TO-INTERPRET :WHAT ?content :REASON (MISSING-ACTIVE-GOAL) :context ?context)
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
				)
		  ))



#||	       
;;   Here are some very specific rules that were used for the Nov demo -- should delete soon
(add-state 'propose-goal-Nov-demo
 (state :action '(SAY-ONE-OF  :content ("What do you want to do?"))
	:transitions (list
		      (transition
		       :description "Let's build a model/I need to find a treatment for cancer"
		       :pattern '((ONT::SPEECHACT ?!sa (? x ONT::PROPOSE ONT::REQUEST) ) ;; :WHAT ?!what) ;; disabled short term
				  (ONT::F ?!what ONT::SCRUTINY :NEUTRAL ?!loc)
				  (TERM ?!loc ONT::DISTRICT :ASSOC-WITH ?!mol)
				  (TERM ?!mod (? type ONT::MOLECULAR-PART))
				  (ont::eval (find-attr (?id ?goal) GOAL))
				  -propose-subgoal-Nov-demo>
				  (RECORD SUBGOAL ?!what)
				  (INVOKE-BA :msg (ONT::EVALUATE :content (ONT::PROPOSE :ps-object ONT::SUBGOAL
						   :content (ONT::BUILD-MODEL :involving (? type ONT::MOLECULAR-PART))
						   :subgoal-of ?id))))
		       :destination 'propose-goal-response
		       :trigger t)
		      )))

(add-state 'propose-goal-response-Nov-demo
	   (state :action nil
		  :transitions (list
				(transition
				 :description "acceptance"
				 :pattern '((ONT::ACCEPT :ps-object ?!psobj :content ?!content :id ?!id)
					    ;;(ont::eval (find-attr ?goal  GOAL))
					    -goal-response1>
					    (RECORD SUBGOAL (?!id ?!content))
					    (generate (ONT::ACCEPT)))
				 
				 :destination 'segment-end)
				(transition
				 :description "rejectance"
				 :pattern '((ONT::REJECT :ps-object ?!psobj :content ?!content )
					    ;;(ont::eval (find-attr ?goal  GOAL))
					    -goal-response2>
					    (RECORD REJECTED (?psobj ?content))
					    (SAY :content "Sorry I can't do that"))
				 :destination 'segment-end)
				)
		  ))

(add-state 'patient-background
 (state :action '(SAY-ONE-OF  :content ("What is the patient's problem?" "what's wrong with the patient?"))
	:transitions (list
		      (transition
		       :description "I have a patient with pancreatic cancer"
		       :pattern '((TERM ?!at ONT::PATIENT)
				  (ONT::F ?!aw ONT::ASSOC-WITH :of ?at :val ?!a)
				  (TERM ?!a (:* ONT::MEDICAL-DISORDERS-AND-CONDITIONS ?disease))
				  (ONT::F ?!r (:* ONT::BODY-PART-VAL ?bodypart) :of ?!a)
				  -patient-background1>
				  (RECORD DISEASE (?disease ?bodypart))
				  (SAY-ONE-OF :content ("OK"))
				  )
		       :trigger t)
		      (transition
		       :description "I have a patient with pancreatic cancer"
		       :pattern '((TERM ?!at ONT::PATIENT)
				  (ONT::F ?!aw ONT::ASSOC-WITH :of ?at :val ?!a)
				  (TERM ?!a (:* ONT::MEDICAL-DISORDERS-AND-CONDITIONS ?disease))
				  -patient-background2>
				  (RECORD DISEASE (?disease))
				  (SAY-ONE-OF :content ("OK"))
				  )
		       :trigger t)
		      )))

(add-state 'establish-goal-identify-drug
 (state :action '(SAY  :content "What is your goal?")
	:transitions (list
		      (transition
		       :description "what drug should we use?"
		       :pattern '((ONT::SPEECHACT ?sa ONT::ASK-WHAT-IS :what ?!foc)
				  (?det ?!foc ONT::MEDICATION :suchthat ?!xx)
				  (ONT::F ?!xx ONT::USE)
				  (ont::eval (find-attr (?disease ?type) DISEASE))
				  -identify-drug1>
				  (RECORD GOAL (FIND-TREATMENT :disease (?disease ?type)))
				  (INVOKE-BA :msg (ONT::EVALUATE :content (ONT::PROPOSE :ps-object ONT::GOAL 
											:content (ONT::FIND-TREATMENT :DISEASE (?disease ?type))))))
		       :destination 'establish-proposal-response
		       :trigger t)
		      #||  This needs to be a PUSH clarification so we can return here when patient problem is identified
		      ;; if we don't the disease, ask  for it
		      (transition
		       :description "What drug should we use"
		       :pattern '((ONT::SPEECHACT ?sa ONT::WH_QUERY :focus ?foc)
				  (?det ?foc ONT::DRUG)
				  -identify-drug2>
				  (RECORD GOAL (FIND-DRUG-FOR ?disease ?type))
				  (SAY-ONE-OF-NEXT :content ("OK"))
				  )
		       :destination patient-background
		       :trigger t)||#
		      )
	))

(add-state 'establish-proposal-response
 (state :action nil
	:transitions (list
		      (transition
		       :description "acceptance"
		       :pattern '((ONT::ACCEPT :ps-object ?!psobj :content ?!content :id ?!id)
				  ;;(ont::eval (find-attr ?goal  GOAL))
				  -proposal-response1>
				  (RECORD ?!psobj  (?!id ?!content))
				  (INVOKE-BA :msg (ONT::PERFORM :id ?!id :content ?!content)))
				              
		       :destination 'establish-solution-response)
		      (transition
		       :description "rejectance"
		       :pattern '((ONT::REJECT :ps-object ?!psobj :content ?!content )
				  ;;(ont::eval (find-attr ?goal  GOAL))
				  -proposal-response2>
				  (RECORD REJECTED (?psobj ?!content))
				  (SAY :content "Sorry I can't do that"))
		       :destination 'segment-end)
		     )
	))

(add-state 'establish-solution-response
	   (state :action nil
		  :transitions (list
				(transition 
				 :description "don't know"
				 :pattern '((ONT::TELL :content (ONT::DONT-KNOW :content (ONT::A ?!x :instance-of ?!class)))
					    -background-situation3>
					    (RECORD BACKGROUND ?!content)
					    (GENERATE (ONT::ASSERT :content 
						       (ONT::TELL :content (ONT::RELN R1 :instance-of ONT::FAMILIAR
										      :force ONT::FALSE
										      :suchthat (A ?!x :instance-of ?!class))))))
				 :destination 'segment-end)
				
				(transition
				 :description "background info"
				 :pattern '((ONT::TELL :content ?!content)
					    -background-situation>
					    (RECORD BACKGROUND ?!content)
					    (GENERATE (ONT::ASSERT :content ?!content)))
				 :destination 'establish-solution-response)

				(transition
				 :description "propose subgoal"
				 :pattern '((ONT::PROPOSE :PS-OBJECT ?!psob :ID ?ID :SUBGOAL-OF ?goal :content ?!content)
					    -establish-solution-response2>
					    (RECORD SUBGOAL ?!content)
					    (GENERATE (ONT::PROPOSE :PS-OBJECT ONT::SUBGOAL :content ?!content)))
				 :destination 'establish-subgoal-response)
				)))



;; define-solution allows a series of USER and SYSTEM additions to the solution (the model in this case)

(add-state 'define-solution
  (state :action '(SAY :content "Anthing else?")
	 :preprocessing-ids '(progression)
	 :transitions (list
	      (transition 
	          :description "Ras activates Raf"
		  :pattern '((ONT::SPEECHACT ?sa ONT::TELL :content ?!con)
			     (ONT::F ?!con ONT::ACTIVATE :agent ?!ag :affected ?!aff :force T)
			     (ont::eval (find-attr ?!goal GOAL))
			     -define-soln1
			     (RECORD MODEL (ONT::ACTIVATE :agent ?ag :affected ?aff))
			     (DISPLAY (ONT::ACTIVATE :agent ?ag :affected ?aff)))
		  :trigger t
		  :destination 'define-solution)
	      (transition 
	          :description "Ras drives cancer progression"
		  :pattern '((ONT::SPEECHACT ?sa ONT::TELL :content ?!con)
			     (ONT::F ?con ONT::CAUSE-EFFECT :agent ?!ag :affected ?!aff :force T)
			     (ont::eval (find-attr ?!goal GOAL))
			     -define-soln2
			     (RECORD MODEL (ONT::CAUSE-EFFECT :agent ?ag :affected ?aff))
			     (DISPLAY (ONT::CAUSE-EFFECT :agent ?ag :affected ?aff)))
		  :trigger t
		  :destination 'define-solution)
	      
	      (transition
	       :description "I'm done"
	       :pattern '((PROGRESSION :value DONE)
			  (ont::eval (find-attr ?!model MODEL))
			  -define-soln3
			  (SAY-ONE-OF :content ("ok" "thanks")))
			  ;;(INVOKE-BA :msg (ONT::DISPLAY-MODEL :model ?!model)
			  ;; :continuation establish-proposal-response))
	       :trigger t
	       :destination 'segment-end)
	       )
	 ))

;; evaluate solution
(add-state 'evaluate-solution
  (state 
   :transitions (list
       (transition 
	 :description "Does Ras activate Raf (if we add Y)"
	 :pattern '((ONT::SPEECHACT ?!sa ONT::SA_YN-QUESION :content ?con)
		    (ONT::F ?con ONT::STOP :agent ?ag :affected ?!aff 
		     :condition ?cond)
		    (ONT::F ?cond ONT::POS-CONDITION :of ?con :val ?condition)
		    (ONT::F ?condition ONT::event-of-action :agent ?doc :affected ?!addee)
		    (ont::eval (find-attr ?!model MODEL))
		    -evaluate-soln1>
		    (SAY :content "Let me check")
		    (RECORD DESIRED-RESULT (ONT::INACTIVATED :affected ?!aff))
		    (RECORD TREATMENT (?condition :affected ?!addee))
		    (INVOKE-BA :msg (ONT::EVALUATE-MODEL :model ?!model 
				     :desired-result (ONT::INACTIVATED :affected ?!aff)
				     :treatment (?condition :affected ?!addee))
		               :continuation evaluate-solution-response))
	 :trigger t
	 :destination 'wait-state)
       )))

;;  handling the response from an evaluation request

(add-state 'evaluate-solution-response
  (state 
   :transitions (list
       ;; success
       (transition 
	:description "yes"
	:pattern '((ONT::SUCCESS)
		   (ont::eval (find-attr ?!result DESIRED-RESULT))
		   -evaluate-soln-response1>
		   (SAY :content "Yes")
		   (GENERATE :content (ONT::ASSERT :content ?!result)))
	:destination 'evaluate-solution-response)
       ;; qualifications
        (transition 
	 :description "but .... (first qualification)"
	 :pattern '((ONT::QUALIFICATION :content ?!content)
		    (ont::eval (find-attr ?!result DESIRED-RESULT))
		    (ont::eval (null (find-attr ?qual QUALIFICATION)))
		    -evaluate-soln-response2>
		    (SAY :content "But")
		    (RECORD QUALIFICATION ?!content)
		    (GENERATE :content (ONT::ASSERT :content ?!content)))
	 :destination 'evaluate-solution-response)

	;; subsequent qualifications (no but)
	(transition 
	 :description "but .... (first qualification)"
	 :pattern '((ONT::QUALIFICATION :content ?!content)
		    (ont::eval (find-attr ?!result DESIRED-RESULT))
		    (ont::eval (find-attr ?!qual QUALIFICATION))
		    -evaluate-soln-response3>
		    (RECORD QUALIFICATION ?!content)
		    (GENERATE :content (ONT::ASSERT :content ?!content)))
	 :destination 'evaluate-solution-response)
	)
   ))
	

			



;; ==== misc things user might say that we don't have to response to
(add-segment 'talk-later
	     (segment
	       :trigger '((ONT::F ?x ONT::TALK)
			  (ONT::F ?y (:* ONT::EVENT-TIME-REL W::LATER))
			  -talk-later1>
			  (true))
	       :start-state 'segmentend
	      ))

(add-segment 'ok
	     (segment
	       :trigger '((ONT::SPEECHACT ?sa (? a ONT::ANSWER ONT::EVALUATE ONT::SA_EVALUATE ONT::FRAGMENT))
			  (ONT::F ?x (:* ONT::GOOD W::OKAY))
			  -ok1>
			  (true))
	       :start-state 'segmentend
	      ))

(add-segment 'cool
	     (segment
	       :trigger '((ONT::F ?x (:* ONT::ACCEPTABILITY-VAL W::COOL))
			  -cool1>
			  (true))
	       :start-state 'segmentend
	      ))

; thanks, bye
(add-segment 'thanks
	     (segment
	       :trigger '((ONT::SPEECHACT ?sa (? a ONT::SA_ACK ONT::SA_THANK ONT::SA_WELCOME ONT::CLOSE))
			  -thanks1>
			  (true))
	       :start-state 'segmentend
	      )
	     )

; good night, goodbye
(add-segment 'thanks
	     (segment
	       :trigger '((ONT::SPEECHACT ?sa ONT::GREET :CONTENT (ONT::GOODBYE :CONTENT ?!c))
			  -goodbye1>
			  (true))
	       :start-state 'segmentend
	      )
	     )

; ok good night
(add-segment 'thanks
	     (segment
	       :trigger '((?spec ?x (:* ONT::CONTENT W::GOOD-NIGHT))
			  -goodbye2>
			  (true))
	       :start-state 'segmentend
	      )
	     )

; ok bye
(add-segment 'thanks
	     (segment
	       :trigger '((?spec ?x (:* ?y W::BYE))  ; bye gets parsed as ONT::ACTING
			  -goodbye3>
			  (true))
	       :start-state 'segmentend
	      )
	     )

(add-state 'checkchannel 
 (state :action '(SAY :content "hello" "i'm here")
  :transitions (list
		(transition
		 :description "Greeting"
		 :pattern '((ONT::SPEECHACT ?sa ONT::GREET :who ?sp) 
			    -checkchannel1> 
			    (SAY-ONE-OF :content ("hello" "hi" "hey" "I'm here.")))
		 :trigger t
		 :destination 'segmentend)
		(transition
		 :description "Hello?"
		 :pattern '((ONT::SPEECHACT ?sa ONT::PROMPT :who ?sp) 
			    -checkchannel2> 
			    (SAY-ONE-OF :content ("I'm here." "I got your message!")))
		 :trigger t
		 :destination 'segmentend)
		(transition
		 :description "I'm up"
		 :pattern '(((? spec ONT::F ONT::BARE) ?x ONT::HAVE-PROPERTY)
			    (ONT::F ?y (:* ONT::SCALE-RELATION W::UP))
			    -checkchannel3> 
			    (SAY-ONE-OF :content ("good morning" "hi!")))
		 :trigger t
		 :destination 'segmentend)
		)
  ))
		


(add-state 'ack (state :action nil
		       :implicit-confirm t
		       :transitions
		       (list
			(transition
			 :description "acknowledge"
			 :pattern '((ONT::F ?!Y (:* ONT::GOOD W::OKAY))
				    -ack1>
				    (TRUE))
			 :destination 'segmentend)
			)))
		  

;;;;;;;;;;
(add-preprocessing 'how-often
 (list 
  (rule
   :description "Not at all"
   :pattern '((ONT::F ?!x (:* ONT::DEGREE-MODIFIER W::NOT-AT-ALL))
	      -how-often1>
	      (HOWOFTEN :value 0))
   )
  (rule
   :description "Not at all"
   :pattern '((ONT::F ?!x (:* ONT::AVAILABILITY-VAL W::AT-ALL))
	      (ONT::F ?!x2 ONT::NEG)
	      -how-often1b>
	      (HOWOFTEN :value 0))
   )
  (rule
   :description "none, two"
   :pattern '((?spec  ?!x ?type :size ?!w)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER ?!w ?n))
	      -how-often2>
	      (HOWOFTEN :value ?n))
   )
  (rule
   :description "five"
   :pattern '((ONT::INDEF-SET  ?!x ?type :number ?!w)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER ?!w ?n))
	      -how-often3>
	      (HOWOFTEN :value ?n))
   )
  (rule
   :description "once"
   :pattern '((ONT::F ?!x (:* ONT::EVENT-TIME-REL w::once))
	      -how-often4>
	      (HOWOFTEN :value 1))
   )
  (rule
   :description "once, twice"
   :pattern '((ONT::F ?!f (:* ONT::REPETITION ?w))
	      (ONT::EVAL (FREQUENCY-TO-NUMBER ?w ?n))
	      -how-often5>
	      (HOWOFTEN :value ?n))
   )
  (rule   ; this is subsumed by how-often2
   :description "A few times, several times"
   :pattern '((ONT::INDEF-SET ?!x  (:* ONT::TIME-POINT ?c) :SIZE ?!size)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER ?!size ?n))
	      -how-often6>
	      (HOWOFTEN :value ?n))
   )
  (rule
   :description "A couple of times"
   :pattern '((ONT::INDEF-SET ?!x  (:* ONT::TIME-POINT ?c))
	      (ONT::F ?!x1  (:* ONT::MODIFIER W::A-COUPLE-OF))
	      (ONT::EVAL (FREQUENCY-TO-NUMBER W::A-COUPLE-OF ?n))
	      -how-often7>
	      (HOWOFTEN :value ?n))
   )
  (rule
   :description "three times"
   :pattern '((?spec ?!x  (? x1 ONT::TIME-INTERVAL ONT::TIME-POINT) :SIZE ?!size)
	      (ONT::A ?!size ONT::NUMBER :value ?!num)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER ?!num ?n))
	      -how-often8>
	      (HOWOFTEN :value ?n))
   )

  (rule
   :description "most of the time"
   :pattern '((?spec ?!size (? x ONT::TIME-INTERVAL ONT::TIME-POINT) :quan ?!q :NEGATION -)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER ?!q ?n))
	      -how-often9>
	      (HOWOFTEN :value ?n))
   )
 
  (rule
   :description "occasionally, frequently, often, never"
   :pattern '((ONT::F ?!x  (:* ONT::FREQUENCY ?freq) :FORCE ONT::TRUE)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER ?freq ?n))
	      -how-often10>
	      (HOWOFTEN :value ?n))
   )

  (rule
   :description "not often"  ; "not occasionally": if they say that, will need another value
   :pattern '((ONT::F ?!x  (:* ONT::FREQUENCY ?freq) :FORCE ONT::FALSE)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER ?freq ?n))
	      -how-often10b>
	      (HOWOFTEN :value 1))
   )
  
  (rule
   :description "not very (often)"
   :pattern '((ONT::F ?!x  (:* ONT::DEGREE-MODIFIER-HIGH W::VERY))
	      (ONT::F ?!y ONT::NEG)
	      -how-often10c>
	      (HOWOFTEN :value 1))
   )

  (rule
   :description "alot, plenty, a little"
   :pattern '((ONT::A ?!x ?type :QUAN ?!q)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER ?!q ?n))
	      -how-often11>
	      (HOWOFTEN :value ?n))
   )

  (rule
   :description "a lot"
   :pattern '((ONT::A ?!x (:* ONT::AREA-DEF-BY-USE W::LOT) :FORCE ONT::TRUE)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER W::a-lot ?n))
	      -how-often12>
	      (HOWOFTEN :value ?n))
   )

  (rule
   :description "not a lot"
   :pattern '((ONT::A ?!x (:* ONT::AREA-DEF-BY-USE W::LOT) :FORCE ONT::FALSE)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER W::not-a-lot ?n))
	      -how-often12b>
	      (HOWOFTEN :value ?n))
   )

  (rule
   :description "a lot"
   :pattern '((ONT::A ?!x (:* ONT::AREA-DEF-BY-USE W::LOT) :NEGATION -)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER W::a-lot ?n))
	      -how-often12c>
	      (HOWOFTEN :value ?n))
   )

  (rule
   :description "not a lot"
   :pattern '((ONT::A ?!x (:* ONT::AREA-DEF-BY-USE W::LOT) :NEGATION +)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER W::not-a-lot ?n))
	      -how-often12d>
	      (HOWOFTEN :value ?n))
   )

  (rule
   :description "a bit"
   :pattern '((Ont::A ?!x ONT::QUANTITY :unit (:* ONT::MEMORY-UNIT ONT::BIT))
	      (ONT::EVAL (FREQUENCY-TO-NUMBER ont::a-bit ?n))
	      -how-often13>
	      (HOWOFTEN :value ?n))
   )
  
  (rule
   :description "one time"
   :pattern '((?spec ?!x  (? x1 ONT::TIME-INTERVAL ONT::TIME-POINT))
	      (?spec2 ?!y (:* ONT::REFERENTIAL-SEM W::ONE))
	      (ONT::EVAL (FREQUENCY-TO-NUMBER W::ONE ?n))
	      -how-often14>
	      (HOWOFTEN :value ?n))
   )

  (rule 
   :description "one"
    :pattern '((?spec2 ?!y (:* ONT::REFERENTIAL-SEM W::ONE))
	      (ONT::EVAL (FREQUENCY-TO-NUMBER W::ONE ?n))
	      -how-often14a>
	       (HOWOFTEN :value ?n)))

  (rule
   :description "zero"
   :pattern '((?spec ?!x  (:* ONT::NUMBER-RELATED-PROPERTY-VAL W::ZERO))
	      (ONT::EVAL (FREQUENCY-TO-NUMBER W::ZERO ?n))
	      -how-often15>
	      (HOWOFTEN :value ?n))
   )

  (rule
   :description "a couple"
   :pattern '((ONT::A ?!x  (:* ONT::QUANTITY -) :UNIT (:* ONT::QUANTITY ONT::COUPLE))
	      (ONT::EVAL (FREQUENCY-TO-NUMBER W::A-COUPLE ?n))
	      -how-often16>
	      (HOWOFTEN :value ?n))
   )

  (rule
   :description "not much"
   :pattern '((ONT::F ?!x (:* ONT::DEGREE-MODIFIER-HIGH W::MUCH))
	      (ONT::F ?!y ONT::NEG)
	      -how-often17>
	      (HOWOFTEN :value 1))
   )

  (rule
   :description "not very much"
   :pattern '((ONT::F ?!x (:* ONT::PREDICATE W::VERY-MUCH))
	      (ONT::F ?!y (:* ONT::NEG W::NOT))
	      -how-often18>
	      (HOWOFTEN :value 1))
   )

  (rule
   :description "very much"
   :pattern '((ONT::F ?!x (:* ONT::PREDICATE W::VERY-MUCH) :FORCE ONT::TRUE)
	      -how-often19>
	      (HOWOFTEN :value 3))
   )

  (rule
   :description "not many times"
   :pattern '((?spec ?!size (? x ONT::TIME-INTERVAL ONT::TIME-POINT) :quan ONT::MANY :NEGATION +)
	      -how-often20>
	      (HOWOFTEN :value 1))
   )

  (rule
   :description "multiple times"
   :pattern '((?spec ?!size (? x ONT::TIME-INTERVAL ONT::TIME-POINT))
	      (ONT::F ?!x2 (:* ONT::FREQUENCY-VAL W::MULTIPLE))
	      -how-often21>
	      (HOWOFTEN :value 3))
   )

  ))
|#

   
;;;;;;;;;;
(add-preprocessing 'yes-no
		   (list
		    (rule
		     :description "no"
		     :pattern '((ONT::SPEECHACT ?!s ONT::ANSWER :what ONT::NEG) 
				-yes-no-no1> 
				(ANSWER :value NO))
		     )
		    (rule
		     :description "not yet"
		     :pattern '((ONT::F ?!s ONT::CONTINUATION) 
				(ONT::F ?!x ONT::NEG)
				-yes-no-no2> 
				(ANSWER :value NO))
		     )
		    (rule
		     :description "I didn't"
		     :pattern '((ONT::F ?!s (:* ONT::ELLIPSIS) :force ONT::FALSE) 
				-yes-no-no3> 
				(ANSWER :value NO)))
		    (rule
		     :description "not now/today"
		     :pattern '((ONT::F ?!s (:* ONT::EVENT-TIME-REL (? w W::NOW W::TODAY))) 
				(ONT::F ?!x ONT::NEG)
				-yes-no-no4> 
				(ANSWER :value NO))
		     )
		    (rule
		     :description "not really"
		     :pattern '((ONT::F ?!s (:* ONT::DEGREE-OF-BELIEF W::REALLY) :FORCE ONT::FALSE) 
				-yes-no-no5> 
				(ANSWER :value NO))
		     )
		    (rule
		     :description "yes"
		     :pattern '((ONT::SPEECHACT ?!s ONT::ANSWER :what ONT::POS) 
				-yes-no-yes1> 
				(ANSWER :value YES)))
		     (rule
		     :description "I did"
		     :pattern '((ONT::F ?!s (:* ONT::ELLIPSIS) :force ONT::TRUE) 
				-yes-no-yes2> 
				(ANSWER :value YES)))
		    
		    ))


#|
(add-preprocessing 'yes-no-others
		   (list
		    ; should really record current time minus 10 minutes
		    ; now only records current time
		    (rule 
		     :description "10 minutes ago; a while ago"
		     :pattern '((?sp ?!x (:* ONT::EVENT-TIME-REL W::AGO)) 
				-yoo1> 
				(ANSWER :value YES))
		     )

		    (rule 
		     :description "I forgot"
		     :pattern '(((? spec ONT::F ONT::BARE) ?!sa (:* ONT::FORGET W::FORGET))
				-yoo6> 
				(ANSWER :value NO))
		     )

		    (rule 
		     :description "ok"
		     :pattern '(((? spec ONT::F ONT::BARE) ?!sa (:* ONT::GOOD W::OKAY))
				-yoo7> 
				(ANSWER :value NO))
		     )


))
    
;;;;;;;;;;      
;;  answering how good/bad questions
(add-preprocessing 'qual-good-bad
 (list 
  (rule
   :description "not bad"
   :pattern '((ONT::F ?!x (:* ONT::BAD ?w));; :mod (?!z))
	      (ONT::F ?!Z (:* ONT::NEG ?!neg))
	      (ONT::EVAL (GOOD-BAD-SCORE ONT::BAD ?w ?!neg  ?score))
	      -qual-good-bad3> 
	      (GOODBAD :value ?score :qual (?!neg ?w))))
  (rule
   :description "not good"
   :pattern '((ONT::F ?!x (:* ONT::GOOD ?w));; :mod (?!z))
	      (ONT::F ?!Z (:* ONT::NEG ?!neg))
	      (ONT::EVAL (GOOD-BAD-SCORE ONT::GOOD ?w ?!neg  ?score))
	      -qual-good-bad4> 
	      (GOODBAD :value ?score :qual (?!neg ?w))))

  (rule
   :description "bad"
   :pattern '((ont::F ?!x (:* ONT::BAD ?w))
	      (ONT::EVAL (GOOD-BAD-SCORE ONT::BAD ?w nil ?score))
	      -qual-good-bad1>
	      (GOODBAD :value ?score :qual ?w)))

  (rule
   :description "good"
   :pattern '((ONT::F ?!x (:* ONT::GOOD ?w))
	      (ONT::EVAL (GOOD-BAD-SCORE ONT::GOOD ?w nil ?score))
	      -qual-good-bad2> 
	      (GOODBAD :value ?score :qual ?w)))	 
   
  (rule
   :description "I'm good/bad, good"
   :pattern '((ONT::F ?!x (:* ont::Acceptability-val ?w)) ;; :MODS (?!m))
	      (ONT::F ?!m (:* ONT::DEGREE-MODIFIER ?mod))
	      (ONT::EVAL (GOOD-BAD-SCORE nil ?w ?mod ?score))
	      -qual-good-bad6>
	      (GOODBAD :value ?score :qual (?mod ?w))))
  
  (rule
   :description "I'm good/bads, good"
   :pattern '((ONT::F ?!x (:* ont::Acceptability-val ?w))
	      (ONT::EVAL (GOOD-BAD-SCORE nil ?w nil ?score))
	      -qual-good-bad5>
	      (GOODBAD :value ?score :qual ?w)))

  (rule
   :description "good"
   :pattern '((ONT::QUANTIFIER ?!x (:* ONT::PROBLEM W::PROBLEM) :QUAN ONT::NONE)
	      (ONT::EVAL (GOOD-BAD-SCORE ONT::GOOD ?w nil ?score))
	      -qual-good-bad7> 
	      (GOODBAD :value ?score :qual ?w)))	 

    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  classifying how much of a quality (e.g., how limited, how happy) on a 0 to 4 scale
(defvar *howmuchqual*)

(setq *howmuchqual*
  ' (
     (0 ; not at all
      ((ONT::F ?!x (:* ONT::DEGREE-MODIFIER-VERYLOW)))
      ((ONT::INDEF-SET ?!x ?t :SIZE ONT::NONE))

      ((ONT::F ?!x (:* ONT::AVAILABILITY-VAL W::AT-ALL))
       (ONT::F ?!x2 ONT::NEG))

      )
     (1  ;; slightly
      ((ONT::F ?!x (:* ONT::DEGREE-MODIFIER (? w W::A-LITTLE W::A-BIT))))
      ((?spec ?!x ?t :quan ont::A-LITTLE))
      ((ONT::F ?!x (:* ONT::DEGREE-MODIFIER (? w W::MUCH W::VERY W::A-LOT))) ;;:MODS (?!y))
       (ONT::F ?!y (:* ONT::NEG W::NOT)))
      ((ONT::F ?!x ONT::LITTLE))
      ((ONT::F ?!x ONT::LITTLE)
       (ONT::F ?!y ONT::DEGREE-MODIFIER :OF ?x!))
      ((Ont::A ?!x ONT::QUANTITY :unit (:* ONT::MEMORY-UNIT ONT::BIT)))  ;;  handle misparsing of "a bit"
      ((ONT::A ?!x (:* ONT::AREA-DEF-BY-USE W::LOT) :NEGATION +))  ;; not a lot
      )

     (2  ;; moderately
      ((ONT::F ?!x (:* ONT::SEVERITY-VAL W::MODERATELY)))
      ((ONT::INDEF-SET ?!x ?t :SIZE ONT::SOME))
      ((ONT::F ?!x (:* ONT::DEGREE-MODIFIER (? w W::SOMEWHAT))))
      )

     (3  ;; very
      ((ONT::F ?!x (:* ONT::DEGREE-MODIFIER (? w W::VERY W::A-LOT W::WELL))))
      ((ONT::INDEF-SET ?!x ?t :SIZE ONT::PLENTY))
      ((ONT::A ?!x (:* ONT::AREA-DEF-BY-USE W::LOT) :NEGATION -))
      
      )

     (4 ;; totally
      ;; all the time (need to fix parser)
      ((ONT::F ?!x (:* ONT::PART-WHOLE-VAL W::TOTALLY)))
      ((ONT::F ?!x (:* ONT::DEGREE-MODIFIER (? w W::A-GREAT-DEAL W::COMPLETELY))))
       
      )
     )
    )

(define-rules 'howmuchqual *howmuchqual* 'howmuchqual :value)


(defun words (var)
  (result var *words*))

(defun current-time (var)
  (result var (get-time-of-day)))

(defun time-before (time)
  (if (t-before (get-time-of-day) time)
      im::*success*))
   
(defun result (n x)
  (im::match-with-subtyping n x))

(defun find-attr (var name)	
  (result var (get-attr *current-user* name)))

||#
