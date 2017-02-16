
;;;;
;;;; File: test.lisp
;;;; Creator: George Ferguson
;;;; Created: Tue Jun 19 14:35:09 2012
;;;; Time-stamp: <Sun Feb 12 11:09:08 CST 2017 lgalescu>

;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up "config" "lisp")
		       :name "trips")))
;;;
;;; Load our TRIPS system
;;;
(load #!TRIPS"src;Systems;cabot;system")

;;;
;;; Load core testing code
;;;
(load #!TRIPS"src;Systems;core;test")

;;;
;;; In USER for convenience of testing
;;;
(in-package :common-lisp-user)

(setf *texttagger-split-mode* nil)

;;; Sample dialogues
;;;
(setf *sample-dialogues*
  '(

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;; The following scripts are (or should be!) working ;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (test-generic .
     ;; system asks for goal; system acts
     ( ;;(TELL :content (SET-SYSTEM-GOAL :content (IDENTIFY :neutral WH-TERM :as (GOAL))
			;;	       :context ((ONT::RELN ONT::PERFORM :what WH-TERM))))
      (TELL :content (SET-SYSTEM-GOAL :id NIL :what NIL
				      :context NIL))
      ;(REQUEST :content (UPDATE-CSM :content (SET-OVERRIDE-INITIATIVE :OVERRIDE T :VALUE T)))

      ;; > INITIATE-CPS-GOAL
      ;; S: What do you want to do?
      ;; > NIL
      "Let's build a 3 step staircase."
      ;; >> PROPOSE-CPS-ACT > HANDLE-CSM-RESPONSE 
      ;; (evaluate) (acceptable) > PROPOSE-CPS-ACT-RESPONSE (commit)
      ;; S: Okay.
      ;; > WHAT-NEXT-INITIATIVE-ON-NEW-GOAL > WHAT-NEXT-INITIATIVE > WHAT-NEXT-INITIATIVE-CSM (initiative:maybe) (what-next)
      ;; (propose) > PERFORM-BA-REQUEST
      ;; S: Put block B6 on the table"
      ;; > ANSWERS
      "ok."
      ;; (commit) > WHAT-NEXT-INITIATIVE > WHAT-NEXT-INITIATIVE-CSM  (initiative:maybe) (what-next) 
      ;; (execution-status:waiting-for-user) > PERFORM-BA-REQUEST
      ;; > CHECK-TIMEOUT-STATUS > NIL
      (TELL :content (REPORT :content (EXECUTION-STATUS :goal G5 :status ONT::DONE)))
      ;; >> EXECUTION-STATUS-HANDLER > WHAT-NEXT-INITIATIVE-ON-NEW-GOAL > WHAT-NEXT-INITIATIVE > WHAT-NEXT-INITIATIVE-CSM (initiative:maybe) (what-next)
      ;; (propose) > PERFORM-BA-REQUEST
      ;; S: Please put B7 on B6.
      ;; > ANSWERS
      "ok."
      ;; (commit) > WHAT-NEXT-INITIATIVE > WHAT-NEXT-INITIATIVE-CSM  (initiative:maybe) (what-next)
      ;; (execution-status:done) > PERFORM-BA-REQUEST
      ;; S: Done ;;** is this now done by the system?!?
      ;; > WHAT-NEXT-INITIATIVE-ON-NEW-GOAL > WHAT-NEXT-INITIATIVE > WHAT-NEXT-INITIATIVE-CSM (initiative:maybe) (what-next)
      ;; (execution-status:done) > PERFORM-BA-REQUEST
      ;; S: Done (the staircase is finished)
      ;; > WHAT-NEXT-INITIATIVE-ON-NEW-GOAL
      ;; S: We are done. (with all tasks)
      ;; FIXME? in the following DAGENT asks (WHAT-NEXT :goal NIL)
      ;; > WHAT-NEXT-INITIATIVE (what-next)
      ;; (propose) ;;** what do we do now?
     ))

    (test-set-system-goal .
     ;; system initiates goal; user acts
     (
      (TELL :content (SET-SYSTEM-GOAL :id G1 :what A1
				      :context ((ONT::RELN A1 :instance-of ONT::CREATE :affected-result st1)
						(ONT::A st1 :instance-of ONT::STAIRS :mod r1)
						(ONT::RELN r1 :instance-of ONT::ASSOC-WITH :figure st1 :ground s1)
						(ONT::KIND s1 :instance-of ONT::STEP :amount 3))))
      ;; S: Let's build a three step staircase.
      "ok."
      ;; S: Put block 6 on the table.
      "ok."
      ))

    (test-who-move .
     ;; system asks for goal; clarification about who's doing the actions
     ( (TELL :content (SET-SYSTEM-GOAL :id NIL :what NIL
				       :context NIL))
      ;;(REQUEST :content (UPDATE-CSM :content (SET-OVERRIDE-INITIATIVE :OVERRIDE T :VALUE T)))
      ;; S: what do you want to do?
      "Let's build a 3 step staircase."
      ;; S: OK
      ;; (PROPOSE
      ;;  :CONTENT (ASK-WH :ID G0 :WHAT A0 :QUERY A1 :AS (QUERY-IN-CONTEXT :GOAL C00011))
      ;;  :CONTEXT ((ONT::RELN A1 :INSTANCE-OF ONT::CAUSE-MOVE :AGENT A0 :AFFECTED A2)
      ;;            (ONT::THE A0 :INSTANCE-OF ONT::PERSON :SUCHTHAT A1)
      ;;            (ONT::THE A2 :INSTANCE-OF ONT::SET :ELEMENT-TYPE ONT::BLOCK)))
      ;; S: who should move the blocks?
      "I will."
      ;; S: [Okay.] Put block B6 on the table"
      "ok."
      ;; S: Done
      ;; S: Please put B7 on B6.
      ))

    (test-not-enough-blocks-and-you-do-it .
     ;; unacceptable goal; system shifts acting responsibility to user
     ( (TELL :content (SET-SYSTEM-GOAL :id NIL :what NIL
				       :context NIL))
      
      ;; S: what do you want to do?
      "Let's build a 5 step staircase."
      ;; >> PROPOSE-CPS-ACT > HANDLE-CSM-RESPONSE
      ;; (evaluate) (unacceptable) > PROPOSE-CPS-ACT-RESPONSE > EXPLORE-ALT-INTERP 
      ;; S: we don't have enough blocks.  What shall we do?
      ;; > NIL
      "Let's build a 3 step staircase."
      ;; >> PROPOSE-CPS-ACT ... (commit)
      ;; S: Okay.
      ;; S: Put block B6 on the table"
      ;; > ANSWERS
      "you do it."
      ;; > NIL
      ;; >> PROPOSE-CPS-ACT > HANDLE-CSM-RESPONSE
      ;; (evaluate) (acceptable-as-modification) > PROPOSE-CPS-ACT-RESPONSE > (commit)
      ;; S: OK
      ;; S: Done (M3 "put B6")
      ;; S: Please put B7 on B6.
      ;; >> ANSWERS
      ))
   

    (flow-action-topgoal-yes .
     ;; user proposes action; system guesses top goal; user accepts; user signals goal-achieved
     (
      "Put block 1 on the table"
      ;; (CSM::failure:missing-goal)
      ;; >> PROPOSE-CPS-ACT > HANDLE-CSM-RESPONSE > CLARIFY-GOAL
      ;; S: are you trying to build something?
      "yes"
      ;; (evaluate) (acceptable) > CONFIRM-GOAL-WITH-BA > (commit)
      ;; S: good!
      ;; > HANDLE-CSM-RESPONSE
      ;; (evaluate) (acceptable) > PROPOSE-CPS-ACT-RESPONSE > (commit)
      ;; S: ok (for PUT subgoal)
      ;; > WHAT-NEXT-INITIATIVE-ON-NEW-GOAL > WHAT-NEXT-INITIATIVE > WHAT-NEXT-INITIATIVE-CSM (initiative:yes) (what-next)
      ;; (ex-status:done) -> PERFORM-BA-REQUEST 
      ;; S: Done! (with "Put block 1 on the table")
      ;; > WHAT-NEXT-INITIATIVE-ON-NEW-GOAL ... (what-next)
      ;; (ex-status:waiting-for-user) > PERFORM-BA-REQUEST
      ;; > CHECK-TIMEOUT-STATUS -> NIL
      "We are done."
      ;; S: good!
      ; FIXME: don't we need a message to tell the BA that we are done? currently only CSM knows this. probably what needs to happen is that DAGENT should send (RELEASE :id G)
      ))

    (flow-action-topgoal-no .
     ;; user proposes action; system guesses top goal; user rejects
     (
      "Put block 1 on the table"
      ;; S: are you trying to build something?
      "no"
      ;; S: What do you want to do?
      ;; -> NIL
      ;; FIXME: DAGENT does all this by itself. is CSM left in a good state?
      ))

    (flow-staircase-user-directed .
     ;; user proposes goal and directs system to complete the task; user indicates goal-achieved
     ("Let's build a 2-step staircase"
      ;; S: ok
      ;; > WHAT-NEXT-INITIATIVE-ON-NEW-GOAL ... (what-next)
      ;; (ex-status:waiting-for-user) > PERFORM-BA-REQUEST -> CHECK-TIMEOUT-STATUS -> NIL
      "Put block 1 on the table"
      ;; (evaluate) (acceptable) > PROPOSE-CPS-ACT-RESPONSE > (commit)
      ;; S: ok
      ;; > WHAT-NEXT-INITIATIVE-ON-NEW-GOAL > WHAT-NEXT-INITIATIVE > WHAT-NEXT-INITIATIVE-CSM (initiative:yes) (what-next)
      ;; (ex-status:done) -> PERFORM-BA-REQUEST 
      ;; S: Done (PUT)
      "Put block 2 on top of block 1"
      ;; S: ok
      ;; S: Done (PUT)
      "Put block 3 next to block 1"
      ;; S: ok
      ;; S: Done (PUT)
      "We're done"
      ;; S: good!
      ; FIXME: don't we need a message to tell the BA that we are done? currently only CSM knows this. probably what needs to happen is that DAGENT should send (RELEASE :id G)
     ))

    (flow-user-directed-with-alarms .
     ;; user proposes goal and directs system to complete the task; user indicates goal-achieved; user waits to trigger alarms
     ((TELL :content (ENABLE-ALARMS))
      "Let's build a 2-step staircase"
      ;; S: ok
      ;; > WHAT-NEXT-INITIATIVE-ON-NEW-GOAL ... (what-next)
      ;; (ex-status:waiting-for-user) > PERFORM-BA-REQUEST -> CHECK-TIMEOUT-STATUS -> NIL
      "Put block 1 on the table"
      ;; (evaluate) (acceptable) > PROPOSE-CPS-ACT-RESPONSE > (commit)
      ;; S: ok
      ;; > WHAT-NEXT-INITIATIVE-ON-NEW-GOAL > WHAT-NEXT-INITIATIVE > WHAT-NEXT-INITIATIVE-CSM (initiative:yes) (what-next)
      ;; (ex-status:done) -> PERFORM-BA-REQUEST 
      ;; S: Done (PUT)
      "Put block 2 on top of block 1"
      ;; S: ok
      ;; S: Done (PUT)
      "Put block 3 next to block 1"
      ;; S: ok
      ;; S: Done (PUT)
      "We're done"
      ;; S: good!
      ; FIXME: don't we need a message to tell the BA that we are done? currently only CSM knows this. probably what needs to happen is that DAGENT should send (RELEASE :id G)
      (TELL :content (DISABLE-ALARMS))
     ))

    (test-alarm-waiting-for-user .
     ;; user proposes goal, system directs and user executes; user takes a long time to perform action, triggering prompts; eventually, user executes action
     ((TELL :content (ENABLE-ALARMS)) 
      "Let's build a 3 step staircase."
      ;; "Put block B6 on the table"
      ;; can wait for timeouts here without interfering with the upcoming answer
      "ok."
      ;; now wait until time out ;; after second time the system says its waiting

      (TELL :content (REPORT :content (EXECUTION-STATUS :goal G5 :status ONT::DONE)))  ;; we know its G5 as the dummy geerated the request
      (TELL :content (DISABLE-ALARMS))
      ))

    (test-alarm-waiting-for-system .
     ;; user proposes goal and actions, system executes actions; system takes a long time to perform action, triggering repeated status checks; eventually, system executes action
     ((TELL :content (ENABLE-ALARMS)) 
      "Let's build a 3 step staircase."
      ;; OK  (system accepts the goal)
      "Put block B6 on the table"
      ;;  OK.
      ;;  wait for timeout here, on the second one it reports it is done
      ;;  and after that the system just continually reports that its waiting for the user
      (TELL :content (DISABLE-ALARMS))
      ))

  
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;; The following test scripts have known problems ;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  

    (test-instead .
     ;; system initiates goal; user rejects it and counter-proposes a new goal
     ( (TELL :content (SET-SYSTEM-GOAL :id G1 :what A1
				       :context ((ONT::RELN A1 :instance-of ONT::CREATE :affected-result st1)
						 (ONT::A st1 :instance-of ONT::STAIRS :mod r1)
						 (ONT::RELN r1 :instance-of ONT::ASSOC-WITH :figure st1 :ground s1)
						 (ONT::KIND s1 :instance-of ONT::STEP :amount 3))))
      ;; S: Let's build a three step staircase.
      ;; > ANSWERS 
      "Let's build a tower instead."
      ;; > NIL 
      ;; >> PROPOSE-CPS-ACT > HANDLE-CSM-RESPONSE
      ;; (evaluate-modification) (acceptable-with-effect) > PROPOSE-CPS-ACT-RESPONSE > (commit)
      ;; > WHAT-NEXT-INITIATIVE-ON-NEW-GOAL > WHAT-NEXT-INITIATIVE > WHAT-NEXT-INITIATIVE-CSM (initiative:maybe) (what-next)
      ;; (propose)
      ;; S: [ok.]  Put block B6 on the table.
      ;; >ANSWERS
      ))

    (test-I-cannot-do-it .
     ;; starts like test-generic, then user refuses to execute action and system asks user to suggest an alternative
     ( (TELL :content (SET-SYSTEM-GOAL :id NIL :what NIL
				       :context NIL))
      ;;(REQUEST :content (UPDATE-CSM :content (SET-OVERRIDE-INITIATIVE :OVERRIDE T :VALUE T)))
      ;; S: What do you want to do?
      "Let's build a 3 step staircase."
      ;; S: Okay.
      ;; S: Put block B6 on the table"
      "ok."
      (TELL :content (REPORT :content (EXECUTION-STATUS :goal G5 :status ONT::DONE)))
      ;; S: Please put B7 on B6.
      ;; > ANSWERS
      "I can't do it."
      ;; (rejected) > WHAT-NEXT-INITIATIVE-ON-NEW-GOAL > WHAT-NEXT-INITIATIVE > WHAT-NEXT-INITIATIVE-CSM  (initiative:maybe) (what-next)
      ;; (propose) > PERFORM-BA-REQUEST
      ;; S: Suggest something else.
      ;; > ANSWERS > NIL (?)
      "Put block 8 on block 6."
      ;; >> PROPOSE-CPS-ACT ... (commit)
      ;; S: ok.
      ;; S: Done (PUT)
      ;; (execution-status:done)
      ;; S: Done. (we built the staircase)
      ;; S: Done. (all tasks accomplished)
      ;; S: What do you want to do? 
      "Let's build a tower."
      ;; S: ok.
      ;; S: Done (Top goal is initiated properly, but we are immediately done because we have exhausted the dummy's ability to say other things.)
      ;; S: Done. (all tasks accomplished)
      ;; S: What do you want to do?
      ))
    
    (test-who-move-user .
     (
      (TELL :content (SET-SYSTEM-GOAL :id NIL :what NIL
				       :context NIL))
      "Let's build a 3 step staircase. who will move the blocks?"
      ;; >> PROPOSE-CPS-ACT ... (commit)
      ;; S: OK
      ;; > WHAT-NEXT-INITIATIVE-ON-NEW-GOAL > WHAT-NEXT-INITIATIVE > NIL
      ;; >> PROPOSE-CPS-ACT > HANDLE-CSM-RESPONSE 
      ;; (evaluate) (acceptable) > PROPOSE-CPS-ACT-RESPONSE (commit)
      ;; > WHAT-NEXT-INITIATIVE-ON-NEW-GOAL > WHAT-NEXT-INITIATIVE > WHAT-NEXT-INITIATIVE-CSM (initiative:yes) (what-next)
      ;; FIXME: the following ANSWER-with-EFFECT is not documented!
      ;; (REPORT :content (ANSWER :TO C00088 :WHAT ONT::V60203 :QUERY ONT::V60222 :VALUE A1
      ;;                   :EFFECT (ADOPT :ID XX1 :WHAT YY1 :AS (MODIFICATION :OF C00086)))
      ;;  :context ((ONT::THE A1 :INSTANCE-OF ONT::PERSON :EQUALS USER)))
      ;; S: I will
      ;;  WHAT-NEXT 
      ;;   (EXECUTION_STATUS  WAITING-FOR-USER)
      "Put B7 on the table"
      ))

    (test-user-move-ynq-as-propose .
     ((TELL :content (SET-SYSTEM-GOAL :id NIL :what NIL
				       :context NIL))
      ;(REQUEST :content (UPDATE-CSM :content (SET-OVERRIDE-INITIATIVE :OVERRIDE T :VALUE T)))
      "Let's build a 3 step staircase. Can I move the blocks?"
      ;; >> PROPOSE-CPS-ACT ... (commit)
      ;; S: OK
      ;; > WHAT-NEXT-INITIATIVE-ON-NEW-GOAL > WHAT-NEXT-INITIATIVE > NIL
      ;; >> PROPOSE-CPS-ACT > HANDLE-CSM-RESPONSE 
      ;; FIXME: CSM tags this as a PROPOSE SUBGOAL.  Either we change this or the BA should send back a MODIFICATION
      ;; (PROPOSE
      ;;  :CONTENT (ADOPT :WHAT A1 :AS (SUBGOAL :OF XXX))
      ;;  :CONTEXT ((ONT::RELN A1 :INSTANCE-OF ONT::CAUSE-MOVE :AGENT A0 :AFFECTED A2)
      ;;            (ONT::THE A0 :INSTANCE-OF ONT::PERSON :SUCHTHAT A1)
      ;;            (ONT::THE A2 :INSTANCE-OF ONT::SET :ELEMENT-TYPE ONT::BLOCK)))
      ))

    (test-user-move-true-ynq .
     ( (TELL :content (SET-SYSTEM-GOAL :id NIL :what NIL
				       :context NIL))
      ;(REQUEST :content (UPDATE-CSM :content (SET-OVERRIDE-INITIATIVE :OVERRIDE T :VALUE T)))
      "Let's build a 3 step staircase. Are there enough blocks?"
      ;; >> PROPOSE-CPS-ACT ... (commit)
      ;; S: ok
      ;; > WHAT-NEXT-INITIATIVE-ON-NEW-GOAL > WHAT-NEXT-INITIATIVE > NIL
      ;; >> PROPOSE-CPS-ACT > HANDLE-CSM-RESPONSE 
      ;; (evaluate) (acceptable) > PROPOSE-CPS-ACT-RESPONSE (commit)
      ;; > WHAT-NEXT-INITIATIVE-ON-NEW-GOAL > WHAT-NEXT-INITIATIVE > WHAT-NEXT-INITIATIVE-CSM (initiative:yes) (what-next)
      ;; (answer)
      ;; S: yes
      ;; > WHAT-NEXT-INITIATIVE-ON-NEW-GOAL > WHAT-NEXT-INITIATIVE > WHAT-NEXT-INITIATIVE-CSM (initiative:maybe) (what-next)
      ;; (propose) > PERFORM-BA-REQUEST
      ;; FIXME: Dummy proposes bad action (put b7 on b6 -- there is no b6!)
      ))

    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;; The following test scripts don't work (we have not started on these yet) ;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (flow-action-underspecified .
     ;; BA asks user to specify object ** We haven't worked on this yet. **
     ("Let's build a 2-step staircase"
      ;; S: ok
      "Put a block on the table"
      ;; S: ok. Which block?
      ;; 
      ))

    (test-undo .
     ;; user undoes accomplished subgoal
     ("Let's build a 2-step staircase"
      ;; S: ok
      "Put a block on the table"
      ;; S: ok.
      "Undo that" ;; also: "Take that back" 
      ;;
      "Put a green block on the table"
      ))

    (test-undo-instead .
     ;; user proposes modification of already accomplished subgoal
     ("Let's build a 2-step staircase"
      ;; S: ok
      "Put a block on the table"
      ;; S: ok. <and does it>
      "Let's use a green block instead"
      ))

    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;; The test scripts beyond this probably wouldn't work ;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (new-sift-demo .
     ( (TELL :content (SET-SYSTEM-GOAL :id NIL :what NIL
				       :context NIL))
      "Let's build a 5 step staircase."
      ;; we don't have enough blocks
      "Let's build a 3 step staircase."
      ;; OK
      ;; who should move the blocks
      "I will."
      ;;  Okay. Put block B6 on the table"
      "ok."
      "ok."  ; This is a stand-in for EXECUTION-STATUS done
      ;; Please put B7 on B6.
      "you do it."
      ;;  OK. 
      ))

    (learning-demo .  
     ("I want to build a tower."
      ;; S: OK. What is a tower?
      "A tower is a structure taller than its diameter"
      ;; S: OK.
	;; <User builds a tower>
      "Is this a tower?"
	;; S: Yes.
	"Now I want to teach you what a row is."
	"This is a row."
	;; S: How many blocks can be in a row?"
	"Any number"
      ;; S: OK. Is this a row?
      "No, a row is a line of blocks."
      ;; S: OK. How about this?
      "Yes, that's a row. Let's build one upwards."
	;; S: How about this?
	"Good. Now add another one."
	;; S: I see we've built a tower.
      )
     )
    (green-staircase-and-add-another-one-demo .
     (
      ;;;;;;;;;;;;;;  Part 1: shared goal discussion ;;;;;;;;;

      ; S: I've been told we need to build a 3 step staircase.
      ; << what's the systems initial model of what a staircase is... >>
      ; PROPOSE (ADOPT BUILD <3-step staircase>)

      "OK."
      ; ACCEPT (ADOPT BUILD <3-step staircase>)

      ;;;;;; <<start possible skip>> ;;;;;

      ; S: Could you tell me what the steps are?
      ; PROPOSE (ADOPT (DEFINE step))

      "The steps are the top surfaces of the staircase."
      ; (implicit ACCEPT (ADOPT (DEFINE step))
      ; ASSERTION :contribute-to (DEFINE step)

      ; S: I see.
      ; (ACCEPT ASSERTION :contribute-to (DEFINE step))
      ; (RELEASE-GOAL (DEFINE-step))

      "What color?"
      ; REQUEST (PROPOSE ?color :in-order-to (REFINE-GOAL (COLOR of <staircase> is ?color))

      ; S: Let's make the steps green. 
      ; (PROPOSE (REFINE-GOAL (COLOR of <steps> is GREEN))

      ;;;;;;  <<end possible skip>> ;;;;;

      ;;;;;;;;;;;;;;  Part 2: Construction (possibly with action on both sides?) ;;;;;;;;;

      "Let's make the bottom row."
      ; INTERPRET-SPEECH-ACT [PROPOSE [Make :affected-result [Row :mod bottom]]]
      ; [CSM recognizes this is a subgoal of BUILD staircase]   i.e., PROPOSE [ADOPT [Make …] :as (SUBGOAL of …]]
													 
      ; S: Can you tell me how to do that?
      ; [PROPOSE [INSTRUCT :agent USER :agent1 SYSTEM :formal [METHOD :figure [Make :affected-result [Row :mod bottom]]

      "First, put a block on the table."
      ; (implicit [ACCEPT [INSTRUCT [METHOD …]]]
      ; (PROPOSE (ADOPT [put block on table] :as [SUBGOAL of [DESCRIBE [METHOD …]]

      ; S: Ok.                                                                  
      ; (ACCEPT (ADOPT [put block on table] :as [SUBGOAL of [DESCRIBE [METHOD …]])
      ; <<send to PROXY>>

      "Put another one next to it."

      "And another one."

      "Great. We've finished the row."
      ; <<system has learned what a row is>>
      ; [GOAL-RELEASE [INSTRUCT [METHOD ...

      "Now add another one."
      ; [PROPOSE ...

      ; <<system adds a two block row>>

      "Great!  And one more."

      ; <<system add top row>>

      "Great! Do you want to build another one?"

      ; S: Why don't you get another one to do it!
      
      )
     )

     (mark-demo .  
      ( (TELL :content (SET-SYSTEM-GOAL :content G1
				      :context ((ONT::RELN G1 :instance-of ONT::CREATE :affected-result st1)
						(ONT::A st1 :instance-of ONT::STAIRS :mod r1)
						(ONT::RELN r1 :instance-of ONT::ASSOC-WITH :figure st1 :ground s1)
						(ONT::KIND s1 :instance-of ONT::STEP :amount 3))))
      
       "OK. Let's make the blocks green"
      ;;  we don't have enough blocks
      "Make the top blocks red"
      ;; OK
       "shall we start with the bottom row"
       ;; OK
       ;;  Put a green block on the table
       "OK"
       ;; Put another one next to it
       "OK"
       ;; Now put a red one next to that one
       "OK. Now let's build the second row. Put a green block on the first green block"
       ;; OK  <<does it>>
       "Great. Now put a green block on top of it"
       ;; Shouldn't that block be red?
       "Oh yes. Make it a red block"
       ;;OK       "Great. Now put a red block on the middle block"
       ;; OK
       "Super. We're done"
       ))

    ;; V 0.1  tests basic functionality
    ;; Send an email message (basically a macro, can be done just
    ;; monitoring and sending keystrokes)
    (cabot-first-demo .  
     ( "A house is a block with a triangle on top"
      "Is this a house?"))
    (cabot-demo .  
     ("I want to show you  house"
      ;; S: OK
      "A house is a block with a triangle on top"
      ;; S: OK
      "Here's an example"
      "This is the roof"
      ;;  S: OK. The triangle is the roof
      "Right"
      "Is this a house?"
      ;; S: No. There is no roof
      "Here's another structure. Is this a house?"
      ;; S: yes
      "Is this a house with a blue roof?"
      ;;S: No it's green.
      )
     )
    (new-demo
     ("Hi"
      ;;  Hello
      "I want to build a row of blocks"
      ;; OK. I need to build a tower.
      "Here’s mine."
      ;;  <<places three blocks on the table in a row>>  need some message if we are to run in dummy mode
      ;; I don’t have enough blocks for my tower.
      ;;I need three blocks.
      "OK. You can reuse one of my blocks"
      ;;      System: Can I move one of your block?
      "No"
      ;;  OK, how’s this
      ;;      System: <<Proxy: build tower with bottom block in original position>>)
      "That's good"))
    (integrated-demo .
     (;; message to CPSA setting goal: invoke BA --> set-goal: build-staircase
      (TELL :content (SET-SYSTEM-GOAL :content G1
				      :context ((A G1 :instance-of ONT::BUILD :affected-result st1)
						(A st1 :instance-of ONT::STAIRCASE))))
      "OK"
      (TELL :CONTENT (SCENE-DESCRIPTION
		:TIMESTAMP "2015-08-17T01:45:09.4123"
		:IS-STABLE T 
		:OBJECTS ()))
      ;; S: Let's build a staircase
    
      ;;  CPSA-> BA - what's next?  BA -> CPSA (put a block a table)
      ;; Put a block on the table
      "OK"
      (TELL :CONTENT (SCENE-DESCRIPTION
		:TIMESTAMP "2015-08-17T01:45:23.1361"
		:IS-STABLE T 
		:OBJECTS ((OBJECT ONT::V8039
				  :CLASS (:* ONT::BLOCK W::BLOCK)
				  :PROPERTIES ()
				  :NAME "starbucks block"
				  :SHAPE (SHAPE-PARAMS 
					  :SIDE_LENGTH .171
					  :FACES ((FACE :NUMBER 1 :COLOR "blue"    :ORIENTATION 1)
					(FACE :NUMBER 2 :COLOR "red" :ORIENTATION 2)
					(FACE :NUMBER 3 :COLOR "green" :ORIENTATION 3)))
				  :EXAPHOR ONT::V1324
				  :POSITION (.3211635 -.4607701 .0855261)
				  :ROTATION (.7057797 .0432996 -0.0432995 -0.7057798)
				  :CONFIDENCE 1.0
				  :VISUAL-ID 12as4123adfSa
				  :TOPLEFT (23 56)
				  :BOTTOMRIGHT (50 88))
			  )
	  ))

      ;;  script has a "here's two" at this stage -- but we'll need to discuss how this should be handled. Better for initial demo to wait for the perceptual component to confirm the performance of the action

            
      (TELL :CONTENT (SCENE-DESCRIPTION
		:TIMESTAMP "2015-08-17T01:45:33.1353"
		:IS-STABLE T 
		:OBJECTS ((OBJECT ONT::V8039
				  :CLASS (:* ONT::BLOCK W::BLOCK)
				  :PROPERTIES ()
				  :NAME "starbucks block"
				  :SHAPE (SHAPE-PARAMS 
					  :SIDE_LENGTH .171
					  :FACES ((FACE :NUMBER 1 :COLOR "blue"    :ORIENTATION 1)
					(FACE :NUMBER 2 :COLOR "red" :ORIENTATION 2)
					(FACE :NUMBER 3 :COLOR "green" :ORIENTATION 3)))
				  :EXAPHOR ONT::V1324
				  :POSITION (.3211635 -.4607701 .0855261)
				  :ROTATION (.7057797 .0432996 -0.0432995 -0.7057798)
				  :CONFIDENCE 1.0
				  :VISUAL-ID 12as4123adfSa
				  :TOPLEFT (23 56)
				  :BOTTOMRIGHT (50 88))
			  (OBJECT ONT::V8040
				  :CLASS (:* ONT::BLOCK W::BLOCK)
				  :PROPERTIES ()
				  :NAME "nvidia block"
				  :SHAPE (SHAPE-PARAMS 
					  :SIDE_LENGTH .171
					  :FACES ((FACE :NUMBER 1 :COLOR "blue"    :ORIENTATION 1)
					(FACE :NUMBER 2 :COLOR "red" :ORIENTATION 2)
					(FACE :NUMBER 3 :COLOR "green" :ORIENTATION 3)))
				  :EXAPHOR ONT::V1325
				  :POSITION (.0604577 -0.41800114 0.0855262)
				  :ROTATION (-0.5148671 -0.4846770 0.4846770 0.51486712)
				  :CONFIDENCE 1.0
				  :VISUAL-ID 12as4dfhfadf5
				  :TOPLEFT (50 30)
				  :BOTTOMRIGHT (60 130)))
	  ))

     
      ;; CPSA -> BA (confirmation, request act done)
      ;; BA-- CPSA -> ACK  (that's the "good")
      ;; BA --> CPSA  push together   (i.e., goal ACHIEVE(TOUCHING(B1, B2)))
      ;; OK. Good. Let's push them together"
      ;;  "move this block?"   TO BE DONE (NO TIME FOR FIRST PASS)
    
;;  "OK"

      ;; CPSA identifies clarification
      ;;  CPSA--> BA: is "move B1" part of  "push together"
      ;;  BA -> YES
      ;; CPSA-> generator (ACK)
      ;; "yes"
      ;;  PERCEPTION -->  TOUCHING(B1, B2)
      (TELL :CONTENT (SCENE-DESCRIPTION
		:TIMESTAMP "2015-08-17T01:45:44.2145"
		:IS-STABLE T 
		:OBJECTS ((OBJECT ONT::V8039
				  :CLASS (:* ONT::BLOCK W::BLOCK)
				  :PROPERTIES ()
				  :NAME "starbucks block"
				  :SHAPE (SHAPE-PARAMS 
					  :SIDE_LENGTH .171
					  :FACES ((FACE :NUMBER 1 :COLOR "blue"    :ORIENTATION 1)
					(FACE :NUMBER 2 :COLOR "red" :ORIENTATION 2)
					(FACE :NUMBER 3 :COLOR "green" :ORIENTATION 3)))
				  :EXAPHOR ONT::V1324
				  :POSITION (.2311635 -.4507701 .0855261)
				  :ROTATION (.7057797 .0432996 -0.0432995 -0.7057798)
				  :CONFIDENCE 1.0
				  :VISUAL-ID 12as4123adfSa
				  :TOPLEFT (23 56)
				  :BOTTOMRIGHT (50 88))
			  (OBJECT ONT::V8040
				  :CLASS (:* ONT::BLOCK W::BLOCK)
				  :PROPERTIES ()
				  :NAME "nvidia block"
				  :SHAPE (SHAPE-PARAMS 
					  :SIDE_LENGTH .171
					  :FACES ((FACE :NUMBER 1 :COLOR "blue"    :ORIENTATION 1)
					(FACE :NUMBER 2 :COLOR "red" :ORIENTATION 2)
					(FACE :NUMBER 3 :COLOR "green" :ORIENTATION 3)))
				  :EXAPHOR ONT::V1325
				  :POSITION (.0604577 -0.41800114 0.0855262)
				  :ROTATION (-0.5148671 -0.4846770 0.4846770 0.51486712)
				  :CONFIDENCE 1.0
				  :VISUAL-ID 12as4dfhfadf5
				  :TOPLEFT (50 30)
				  :BOTTOMRIGHT (60 130)))
	  ))
      ;;  CPSA -> BA (what's next)
      ;;  BA -> CPSA: Achieve (ON(?block, B1)
      ;; CPSA->Generator: PUTON((a block), B1)
      ;; Put another block on top of that one
      "OK"
      ;;  CPSA -> Acknowledgement
      ;; PERCEPTION:  On(B3, B1)
      (TELL :CONTENT (SCENE-DESCRIPTION
		:TIMESTAMP "2015-08-17T01:45:58.0941"
		:IS-STABLE T 
		:OBJECTS ((OBJECT ONT::V8039
				  :CLASS (:* ONT::BLOCK W::BLOCK)
				  :PROPERTIES ()
				  :NAME "starbucks block"
				  :SHAPE (SHAPE-PARAMS 
					  :SIDE_LENGTH .171
					  :FACES ((FACE :NUMBER 1 :COLOR "blue"    :ORIENTATION 1)
					(FACE :NUMBER 2 :COLOR "red" :ORIENTATION 2)
					(FACE :NUMBER 3 :COLOR "green" :ORIENTATION 3)))
				  :EXAPHOR ONT::V1324
				  :POSITION (.2311635 -.4507701 .0855261)
				  :ROTATION (.7057797 .0432996 -0.0432995 -0.7057798)
				  :CONFIDENCE 1.0
				  :VISUAL-ID 12as4123adfSa
				  :TOPLEFT (23 56)
				  :BOTTOMRIGHT (50 88))
			  (OBJECT ONT::V8040
				  :CLASS (:* ONT::BLOCK W::BLOCK)
				  :PROPERTIES ()
				  :NAME "nvidia block"
				  :SHAPE (SHAPE-PARAMS 
					  :SIDE_LENGTH .171
					  :FACES ((FACE :NUMBER 1 :COLOR "blue"    :ORIENTATION 1)
					(FACE :NUMBER 2 :COLOR "red" :ORIENTATION 2)
					(FACE :NUMBER 3 :COLOR "green" :ORIENTATION 3)))
				  :EXAPHOR ONT::V1325
				  :POSITION (.0604577 -0.41800114 0.0855262)
				  :ROTATION (-0.5148671 -0.4846770 0.4846770 0.51486712)
				  :CONFIDENCE 1.0
				  :VISUAL-ID 12as4dfhfadf5
				  :TOPLEFT (50 30)
				  :BOTTOMRIGHT (60 130))
			  (OBJECT ONT::V8041
				  :CLASS (:* ONT::BLOCK W::BLOCK)
				  :PROPERTIES ()
				  :NAME "mcdonalds block"
				  :SHAPE (SHAPE-PARAMS 
					  :SIDE_LENGTH .171
					  :FACES ((FACE :NUMBER 1 :COLOR "blue"    :ORIENTATION 1)
					(FACE :NUMBER 2 :COLOR "red" :ORIENTATION 2)
					(FACE :NUMBER 3 :COLOR "green" :ORIENTATION 3)))
				  :EXAPHOR ONT::V1346
				  :POSITION (.2103044 -0.4407028 .2222769)
				  :ROTATION (.05758480 0.6951172 0.7131223 -0.0199164)
				  :CONFIDENCE 1.0
				  :VISUAL-ID 12as4iyuhiop
				  :TOPLEFT (23 50)
				  :BOTTOMRIGHT (50 150)))
	  ))
      ;; CPSA-> BA (WHAT'S NEXT)
      ;; BA -> CPSA : DONE(build-staircase)
      ;; S: OK. We are done
      ))
    
    (integrated-demo-another-one .
     (;; message to CPSA setting goal: invoke BA --> set-goal: build-staircase
      (TELL :content (SET-SYSTEM-GOAL :content G1
				      :context ((A G1 :instance-of ONT::BUILD :affected-result st1)
						(A st1 :instance-of ONT::STAIRCASE))))
      "OK"
      (TELL :CONTENT (SCENE-DESCRIPTION
		:TIMESTAMP "2015-08-17T01:45:09.4123"
		:IS-STABLE T 
		:OBJECTS ()))
      ;; S: Let's build a staircase
    
      ;;  CPSA-> BA - what's next?  BA -> CPSA (put a block a table)
      ;; Put a block on the table
      "OK"
      (TELL :CONTENT (SCENE-DESCRIPTION
		:TIMESTAMP "2015-08-17T01:45:23.1361"
		:IS-STABLE T 
		:OBJECTS ((OBJECT ONT::V8039
				  :CLASS (:* ONT::BLOCK W::BLOCK)
				  :PROPERTIES ()
				  :NAME "starbucks block"
				  :SHAPE (SHAPE-PARAMS 
					  :SIDE_LENGTH .171
					  :FACES ((FACE :NUMBER 1 :COLOR "blue"    :ORIENTATION 1)
					(FACE :NUMBER 2 :COLOR "red" :ORIENTATION 2)
					(FACE :NUMBER 3 :COLOR "green" :ORIENTATION 3)))
				  :EXAPHOR ONT::V1324
				  :POSITION (.3211635 -.4607701 .0855261)
				  :ROTATION (.7057797 .0432996 -0.0432995 -0.7057798)
				  :CONFIDENCE 1.0
				  :VISUAL-ID 12as4123adfSa
				  :TOPLEFT (23 56)
				  :BOTTOMRIGHT (50 88))
			  )
	  ))

      "can I add another one?" 

      ;;  script has a "here's two" at this stage -- but we'll need to discuss how this should be handled. Better for initial demo to wait for the perceptual component to confirm the performance of the action
 ;     (TELL :content (UPDATE-WORLD :content ((RELN t1 :instance-of ONT::TOUCH :neutral b1 :neutral1 b2 :force ont::FALSE)
;					     (A b1 :instance-of ont::BLOCK) (A B2 :instance-of ont::block) )))
      (TELL :CONTENT (SCENE-DESCRIPTION
		:TIMESTAMP "2015-08-17T01:45:33.1353"
		:IS-STABLE T 
		:OBJECTS ((OBJECT ONT::V8039
				  :CLASS (:* ONT::BLOCK W::BLOCK)
				  :PROPERTIES ()
				  :NAME "starbucks block"
				  :SHAPE (SHAPE-PARAMS 
					  :SIDE_LENGTH .171
					  :FACES ((FACE :NUMBER 1 :COLOR "blue"    :ORIENTATION 1)
					(FACE :NUMBER 2 :COLOR "red" :ORIENTATION 2)
					(FACE :NUMBER 3 :COLOR "green" :ORIENTATION 3)))
				  :EXAPHOR ONT::V1324
				  :POSITION (.3211635 -.4607701 .0855261)
				  :ROTATION (.7057797 .0432996 -0.0432995 -0.7057798)
				  :CONFIDENCE 1.0
				  :VISUAL-ID 12as4123adfSa
				  :TOPLEFT (23 56)
				  :BOTTOMRIGHT (50 88))
			  (OBJECT ONT::V8040
				  :CLASS (:* ONT::BLOCK W::BLOCK)
				  :PROPERTIES ()
				  :NAME "nvidia block"
				  :SHAPE (SHAPE-PARAMS 
					  :SIDE_LENGTH .171
					  :FACES ((FACE :NUMBER 1 :COLOR "blue"    :ORIENTATION 1)
					(FACE :NUMBER 2 :COLOR "red" :ORIENTATION 2)
					(FACE :NUMBER 3 :COLOR "green" :ORIENTATION 3)))
				  :EXAPHOR ONT::V1325
				  :POSITION (.0604577 -0.41800114 0.0855262)
				  :ROTATION (-0.5148671 -0.4846770 0.4846770 0.51486712)
				  :CONFIDENCE 1.0
				  :VISUAL-ID 12as4dfhfadf5
				  :TOPLEFT (50 30)
				  :BOTTOMRIGHT (60 130)))
	  ))


      ;; CPSA -> BA (confirmation, request act done)
      ;; BA-- CPSA -> ACK  (that's the "good")
      ;; BA --> CPSA  push together   (i.e., goal ACHIEVE(TOUCHING(B1, B2)))
      ;; OK. Good. Let's push them together"
      ;;  "move this block?"   TO BE DONE (NO TIME FOR FIRST PASS)
      "OK"
      ;; CPSA identifies clarification
      ;;  CPSA--> BA: is "move B1" part of  "push together"
      ;;  BA -> YES
      ;; CPSA-> generator (ACK)
      ;; "yes"
      ;;  PERCEPTION -->  TOUCHING(B1, B2)
      (TELL :CONTENT (SCENE-DESCRIPTION
		:TIMESTAMP "2015-08-17T01:45:44.2145"
		:IS-STABLE T 
		:OBJECTS ((OBJECT ONT::V8039
				  :CLASS (:* ONT::BLOCK W::BLOCK)
				  :PROPERTIES ()
				  :NAME "starbucks block"
				  :SHAPE (SHAPE-PARAMS 
					  :SIDE_LENGTH .171
					  :FACES ((FACE :NUMBER 1 :COLOR "blue"    :ORIENTATION 1)
					(FACE :NUMBER 2 :COLOR "red" :ORIENTATION 2)
					(FACE :NUMBER 3 :COLOR "green" :ORIENTATION 3)))
				  :EXAPHOR ONT::V1324
				  :POSITION (.2311635 -.4507701 .0855261)
				  :ROTATION (.7057797 .0432996 -0.0432995 -0.7057798)
				  :CONFIDENCE 1.0
				  :VISUAL-ID 12as4123adfSa
				  :TOPLEFT (23 56)
				  :BOTTOMRIGHT (50 88))
			  (OBJECT ONT::V8040
				  :CLASS (:* ONT::BLOCK W::BLOCK)
				  :PROPERTIES ()
				  :NAME "nvidia block"
				  :SHAPE (SHAPE-PARAMS 
					  :SIDE_LENGTH .171
					  :FACES ((FACE :NUMBER 1 :COLOR "blue"    :ORIENTATION 1)
					(FACE :NUMBER 2 :COLOR "red" :ORIENTATION 2)
					(FACE :NUMBER 3 :COLOR "green" :ORIENTATION 3)))
				  :EXAPHOR ONT::V1325
				  :POSITION (.0604577 -0.41800114 0.0855262)
				  :ROTATION (-0.5148671 -0.4846770 0.4846770 0.51486712)
				  :CONFIDENCE 1.0
				  :VISUAL-ID 12as4dfhfadf5
				  :TOPLEFT (50 30)
				  :BOTTOMRIGHT (60 130)))
	  ))
      ;;  CPSA -> BA (what's next)
      ;;  BA -> CPSA: Achieve (ON(?block, B1)
      ;; CPSA->Generator: PUTON((a block), B1)
      ;; Put another block on top of that one
      "OK"
      ;;  CPSA -> Acknowledgement
      ;; PERCEPTION:  On(B3, B1)
      (TELL :CONTENT (SCENE-DESCRIPTION
		:TIMESTAMP "2015-08-17T01:45:58.0941"
		:IS-STABLE T 
		:OBJECTS ((OBJECT ONT::V8039
				  :CLASS (:* ONT::BLOCK W::BLOCK)
				  :PROPERTIES ()
				  :NAME "starbucks block"
				  :SHAPE (SHAPE-PARAMS 
					  :SIDE_LENGTH .171
					  :FACES ((FACE :NUMBER 1 :COLOR "blue"    :ORIENTATION 1)
					(FACE :NUMBER 2 :COLOR "red" :ORIENTATION 2)
					(FACE :NUMBER 3 :COLOR "green" :ORIENTATION 3)))
				  :EXAPHOR ONT::V1324
				  :POSITION (.2311635 -.4507701 .0855261)
				  :ROTATION (.7057797 .0432996 -0.0432995 -0.7057798)
				  :CONFIDENCE 1.0
				  :VISUAL-ID 12as4123adfSa
				  :TOPLEFT (23 56)
				  :BOTTOMRIGHT (50 88))
			  (OBJECT ONT::V8040
				  :CLASS (:* ONT::BLOCK W::BLOCK)
				  :PROPERTIES ()
				  :NAME "nvidia block"
				  :SHAPE (SHAPE-PARAMS 
					  :SIDE_LENGTH .171
					  :FACES ((FACE :NUMBER 1 :COLOR "blue"    :ORIENTATION 1)
					(FACE :NUMBER 2 :COLOR "red" :ORIENTATION 2)
					(FACE :NUMBER 3 :COLOR "green" :ORIENTATION 3)))
				  :EXAPHOR ONT::V1325
				  :POSITION (.0604577 -0.41800114 0.0855262)
				  :ROTATION (-0.5148671 -0.4846770 0.4846770 0.51486712)
				  :CONFIDENCE 1.0
				  :VISUAL-ID 12as4dfhfadf5
				  :TOPLEFT (50 30)
				  :BOTTOMRIGHT (60 130))
			  (OBJECT ONT::V8041
				  :CLASS (:* ONT::BLOCK W::BLOCK)
				  :PROPERTIES ()
				  :NAME "mcdonalds block"
				  :SHAPE (SHAPE-PARAMS 
					  :SIDE_LENGTH .171
					  :FACES ((FACE :NUMBER 1 :COLOR "blue"    :ORIENTATION 1)
					(FACE :NUMBER 2 :COLOR "red" :ORIENTATION 2)
					(FACE :NUMBER 3 :COLOR "green" :ORIENTATION 3)))
				  :EXAPHOR ONT::V1346
				  :POSITION (.2103044 -0.4407028 .2222769)
				  :ROTATION (.05758480 0.6951172 0.7131223 -0.0199164)
				  :CONFIDENCE 1.0
				  :VISUAL-ID 12as4iyuhiop
				  :TOPLEFT (23 50)
				  :BOTTOMRIGHT (50 150)))
	  ))
      ;; CPSA-> BA (WHAT'S NEXT)
      ;; BA -> CPSA : DONE(build-staircase)
      ;; S: OK. We are done
      )
     )    
    )
)

;; see test-utterance-demo sample dialogue below
(defun arbitrary-function-to-be-called ()
  (format t "the test-utterance-demo sample dialogue called this arbitrary function~%")
  ;; Note: use COMM:send and not dfc:send-msg, since we're not in the context
  ;; of a defcomponent TRIPS module.
  (COMM:send 'test '(tell :content (message from arbitrary function)))
  ;; For the same reason, we don't have dfc:send-and-wait. Instead, loop over
  ;; COMM:recv and discard messages until you get the reply.
  (COMM:send 'test '(request
		      :receiver lexiconmanager
		      :content (get-lf w::end)
		      :reply-with test123))
  (loop for incoming = (COMM:recv 'test)
  	while incoming
	until (eq 'test123 (util:find-arg-in-act incoming :in-reply-to))
	finally (format t "(get-lf w::end) returned ~s~%"
			(util:find-arg-in-act incoming :content))
	)
  )

;; demo extra capabilities of test-utterance function
;; Note: we have to push this separately because including the #'function
;; doesn't work in a quoted context like the *sample-dialogues* list above.
;; Everything else does.
(push 
  `(test-utterance-demo . (
      "Send a string to be parsed."
      (tell :content (send a single arbitrary message))
      ( (tell :content (send arbitrary list of))
        (tell :content (kqml messages))
	)
      ,#'arbitrary-function-to-be-called
      ))
  *sample-dialogues*)

;; Default sample dialogue for this domain

(defvar *test-dialog-id* 'test-generic)

(setf *test-dialog*
  (cdr (assoc *test-dialog-id* *sample-dialogues*)))


;(setf *test-dialog*
;  (cdr (assoc 0.1 *sample-dialogues* :test #'eql)))

(defun ptest (key)
  "Make the sample dialogue given by KEY the global *TEST-DIALOG*, then
call TEST. Reports available KEYs on error."
  (let ((dialogue (cdr (assoc key *sample-dialogues* :test #'eql))))
    (cond
     ((not dialogue)
      (format t "~&ptest: unknown sample dialogue: ~S~%" key)
      (format t "~&ptest: possible values: ~S~%" (mapcar #'car *sample-dialogues*)))
     (t
      (setf *test-dialog* dialogue)
      (setf *test-dialog-id* key)
      (setf dummy::*replycounter* 0)
      (start-conversation)
      (COMM::send 'test `(TELL :content (component-status :who TEST :what (TESTING ,key))))
      (test)))))


(defun enable-graphviz-display ()
  (COMM::send 'test '(request :receiver graphviz :content (enable-display))))

(defun disable-graphviz-display ()
  (COMM::send 'test '(request :receiver graphviz :content (disable-display))))


