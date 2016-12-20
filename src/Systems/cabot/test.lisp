
;;;;
;;;; File: test.lisp
;;;; Creator: George Ferguson
;;;; Created: Tue Jun 19 14:35:09 2012
;;;; Time-stamp: <Mon Dec 19 08:34:33 EST 2016 jallen>
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

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;; The following scripts are (or should be!) working ;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (test-generic .
     ( ;(TELL :content (SET-SYSTEM-GOAL :content (IDENTIFY :neutral WH-TERM :as (GOAL))
				      ; :context ((ONT::RELN ONT::PERFORM :what WH-TERM))))
      (TELL :content (SET-SYSTEM-GOAL :content NIL
				       :context NIL))
      ;(REQUEST :content (UPDATE-CSM :content (SET-OVERRIDE-INITIATIVE :OVERRIDE T :VALUE T)))
      "Let's build a 3 step staircase."
      ;;  Okay. Put block B6 on the table"
      "ok."
      ;; Please put B7 on B6.
      "ok."
      ;;  We are done.
      ))

    (test-set-system-goal .  
      ( (TELL :content (SET-SYSTEM-GOAL :content G1
				      :context ((ONT::RELN G1 :instance-of ONT::CREATE :affected-result st1)
						(ONT::A st1 :instance-of ONT::STAIRS :mod r1)
						(ONT::RELN r1 :instance-of ONT::ASSOC-WITH :figure st1 :ground s1)
						(ONT::KIND s1 :instance-of ONT::STEP :amount 3))))
	;; Let's build a three step staircase.
	"ok."
	;; Put block 6 on the table.
	"ok."
	;; Please put B7 on B6.
	"ok."
	;;  We are done.
	))

    (test-who-move .
     ( (TELL :content (SET-SYSTEM-GOAL :content NIL
				       :context NIL))
      ;(REQUEST :content (UPDATE-CSM :content (SET-OVERRIDE-INITIATIVE :OVERRIDE T :VALUE T)))
      "Let's build a 3 step staircase."
      ;; OK
      ;; (PROPOSE
      ;;  :CONTENT (ASK-WH :WHAT A0 :QUERY A1 :AS (QUERY-IN-CONTEXT :GOAL ONT::V33355))
      ;;  :CONTEXT ((ONT::RELN A1 :INSTANCE-OF ONT::CAUSE-MOVE :AGENT A0 :AFFECTED A2)
      ;;            (ONT::THE A0 :INSTANCE-OF ONT::PERSON :SUCHTHAT A1)
      ;;            (ONT::THE A2 :INSTANCE-OF ONT::SET :ELEMENT-TYPE ONT::BLOCK)))
      ;; S: who should move the blocks
      "I will."
      ;;  Okay. Put block B6 on the table"
      "ok."
      ;; Please put B7 on B6.
      "ok."
      ;;  We are done.
      ))

    (test-not-enough-blocks-and-you-do-it .
     ( (TELL :content (SET-SYSTEM-GOAL :content NIL
				       :context NIL))
      ;(REQUEST :content (UPDATE-CSM :content (SET-OVERRIDE-INITIATIVE :OVERRIDE T :VALUE T)))
      "Let's build a 5 step staircase."
      ;; we don't have enough blocks
      "Let's build a 3 step staircase."
      ;;  Okay. Put block B6 on the table"
      "ok."
      ;; Please put B7 on B6.
      "you do it."
      ;;  ok.
      ;; We are done.  
      ))

    ;; LG 2016/12/02 FST flow tests
    #|| LG 2016/12/06 individual actions are not allowed anymore!
    (flow-action .
     ;; 
     (
      "Put block 1 on the table"
      ;; propose-cps-act -> handle-csm-response -> propose-cps-act-response
      ;; S: ok
      ;; -> what-next-initiative -> what-next-initiative-csm -> perform-ba-request 
      ;; S: Done!
      ;; -> what-next-initiative-on-new-goal
      ;; S: All tasks completed.
      ;; S: What do we do now?
      ;; -> segmentend
      ))
    ||#

    (flow-action-topgoal-yes .
     ;; user proposes action; system guesses top goal; user accepts
     (
      "Put block 1 on the table"
      ;; PROPOSE-CPS-ACT -> HANDLE-CSM-RESPONSE -> CLARIFY-GOAL
      ;; S: are you trying to build something?  (Note: Currently we don't ask *what* the user is building.)
      "yes"
      ;; -> CONFIRM-GOAL-WITH-BA
      ;; S: good!
      ;; -> HANDLE-CSM-RESPONSE
      ;; -> PROPOSE-CPS-ACT-RESPONSE
      ;; S: ok (for PUT subgoal)
      ;; -> WHAT-NEXT-INITIATIVE -> WHAT-NEXT-INITIATIVE-CSM
      ;; -> PERFORM-BA-REQUEST 
      ;; S: Done! (i.e., done with "Put block 1 on the table")
      ;; -> WHAT-NEXT-INITIATIVE-ON-NEW-GOAL -> WHAT-NEXT-INITIATIVE
      ;; -> WHAT-NEXT-INITIATIVE-CSM
      ;; -> PERFORM-BA-REQUEST -> CHECK-TIMEOUT-STATUS -> NIL
      "We are done."
      ;; S: good!

      ; TOFIX: need a message to tell the BA that we are done

      ;; S: what do you want to do?
      ;; -> DONE -> NIL
      ))

    (flow-action-topgoal-no .
     ;; user proposes action; system guesses top goal; user rejects
     (
      "Put block 1 on the table"
      ;; PROPOSE-CPS-ACT -> HANDLE-CSM-RESPONSE -> CLARIFY-GOAL
      ;; S: are you trying to build something?
      "no"
      ;; S: What do you want to do?
      ;; -> PROPOSE-CPS-ACT
      ))

    (flow-staircase-user-directed .
     ;; user proposes goal and directs system to complete the task
     ("Let's build a 2-step staircase"
      ;; PROPOSE-CPS-ACT -> HANDLE-CSM-RESPONSE
      ;; -> PROPOSE-CPS-ACT-RESPONSE
      ;; S: ok
      ;; -> WHAT-NEXT-INITIATIVE -> WHAT-NEXT-INITIATIVE-CSM
      ;; DUMMY-BA says: WAITING-FOR-USER
      ;; -> PERFORM-BA-REQUEST -> CHECK-TIMEOUT-STATUS -> NIL
      "Put block 1 on the table"
      ;; PROPOSE-CPS-ACT -> HANDLE-CSM-RESPONSE
      ;; -> PROPOSE-CPS-ACT-RESPONSE
      ;; S: ok
      ;; -> WHAT-NEXT-INITIATIVE -> WHAT-NEXT-INITIATIVE-CSM
      ;; DUMMY-BA: EXECUTION-STATUS::DONE (PUT)
      ;; -> PERFORM-BA-REQUEST
      ;; S: Done (PUT)
      ;; -> WHAT-NEXT-INITIATIVE-ON-NEW-GOAL
      ;; -> WHAT-NEXT-INITIATIVE -> WHAT-NEXT-INITIATIVE-CSM
      ;; DUMMY-BA says: WAITING-FOR-USER
      ;; -> PERFORM-BA-REQUEST -> CHECK-TIMEOUT-STATUS -> NIL
      "Put block 2 on top of block 1"
      ;; same as above
      ;; S: ok
      ;; DUMMY-BA: EXECUTION-STATUS::DONE (PUT)
      ;; S: Done (PUT)
      "Put block 3 next to block 1"
      ;; same as above
      ;; S: ok
      ;; DUMMY-BA: EXECUTION-STATUS::DONE (PUT)
      ;; S: Done (PUT)
      "We're done"
      ;; S: good!
      ;; S: what do you want to do?
      ;; -> DONE -> NIL
     ))

    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;; The following test scripts have known problems ;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (alarm-test .
     ("Let's build a 3 step staircase."
      ;; "Put block B6 on the table"
      "ok."
      ;; now wait for system to say "I'm still working on it"

      ;; TOFIX: the alarm is set but doesn't go off.
      ))

    (test-instead .
      ( (TELL :content (SET-SYSTEM-GOAL :content G1
				      :context ((ONT::RELN G1 :instance-of ONT::CREATE :affected-result st1)
						(ONT::A st1 :instance-of ONT::STAIRS :mod r1)
						(ONT::RELN r1 :instance-of ONT::ASSOC-WITH :figure st1 :ground s1)
						(ONT::KIND s1 :instance-of ONT::STEP :amount 3))))
	;; Let's build a three step staircase.
	"Let's build a tower instead."
	
	;; TOFIX: CSM currently sends back MISSING-GOAL-TO-MODIFY

	;; ok.
	))

    (test-I-cannot-do-it .
     ( (TELL :content (SET-SYSTEM-GOAL :content NIL
				       :context NIL))
      ;(REQUEST :content (UPDATE-CSM :content (SET-OVERRIDE-INITIATIVE :OVERRIDE T :VALUE T)))
      "Let's build a 3 step staircase."
      ;;  Okay. Put block B6 on the table"
      "ok."
      ;; Please put B7 on B6.
      "I can't do it."

      ;; TOFIX: need to notify BA of rejection.
      
      ;; What do you want to do?
      "Put block 8 on block 6."
      ;; ok.
      ;; We are done.  What do you want to do?
      "Let's build a tower."
      ;; ok.
      ;; Done (Top goal is initiated properly, but we are immediately done because we have exhausted the dummy's ability to say other things.)
      ))
    
    (test-who-move-user .
     ( (TELL :content (SET-SYSTEM-GOAL :content NIL
				       :context NIL))
      ;(REQUEST :content (UPDATE-CSM :content (SET-OVERRIDE-INITIATIVE :OVERRIDE T :VALUE T)))
      "Let's build a 3 step staircase. who will move the blocks?"
      ;; OK
      ;; (PROPOSE
      ;;  :CONTENT (ASK-WH :WHAT A0 :QUERY A1 :AS (QUERY-IN-CONTEXT :GOAL ONT::V33355))
      ;;  :CONTEXT ((ONT::RELN A1 :INSTANCE-OF ONT::HAUL :AGENT A0 :AFFECTED A2)
      ;;            (ONT::THE A0 :INSTANCE-OF ONT::PERSON :SUCHTHAT A1)
      ;;            (ONT::THE A2 :INSTANCE-OF ONT::SET :ELEMENT-TYPE ONT::BLOCK)))

      ;; TOFIX: need to change to conform to spec
      
      ))

    (test-user-move-ynq-as-propose .
     ( (TELL :content (SET-SYSTEM-GOAL :content NIL
				       :context NIL))
      ;(REQUEST :content (UPDATE-CSM :content (SET-OVERRIDE-INITIATIVE :OVERRIDE T :VALUE T)))
      "Let's build a 3 step staircase. Can I move the blocks?"
      ;; OK
      ;; (PROPOSE
      ;;  :CONTENT (ASK-WH :WHAT A0 :QUERY A1 :AS (QUERY-IN-CONTEXT :GOAL ONT::V33355))
      ;;  :CONTEXT ((ONT::RELN A1 :INSTANCE-OF ONT::HAUL :AGENT A0 :AFFECTED A2)
      ;;            (ONT::THE A0 :INSTANCE-OF ONT::PERSON :SUCHTHAT A1)
      ;;            (ONT::THE A2 :INSTANCE-OF ONT::SET :ELEMENT-TYPE ONT::BLOCK)))

      ;; TOFIX: CSM tags this as a PROPOSE SUBGOAL.  Either we change this or the BA should send back a MODIFICATION
      ))

    (test-user-move-true-ynq .
     ( (TELL :content (SET-SYSTEM-GOAL :content NIL
				       :context NIL))
      ;(REQUEST :content (UPDATE-CSM :content (SET-OVERRIDE-INITIATIVE :OVERRIDE T :VALUE T)))
      "Let's build a 3 step staircase. Are there enough blocks?"
      ;; OK
      ;; (PROPOSE
      ;;  :CONTENT (ASK-WH :WHAT A0 :QUERY A1 :AS (QUERY-IN-CONTEXT :GOAL ONT::V33355))
      ;;  :CONTEXT ((ONT::RELN A1 :INSTANCE-OF ONT::HAUL :AGENT A0 :AFFECTED A2)
      ;;            (ONT::THE A0 :INSTANCE-OF ONT::PERSON :SUCHTHAT A1)
      ;;            (ONT::THE A2 :INSTANCE-OF ONT::SET :ELEMENT-TYPE ONT::BLOCK)))

      ;; TOFIX: need to change to conform to spec
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


    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;; The test scripts beyond this probably wouldn't work ;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (new-sift-demo .
     ( (TELL :content (SET-SYSTEM-GOAL :content (IDENTIFY :neutral WH-TERM :as (GOAL))
				       :context ((ONT::RELN ONT::PERFORM :what WH-TERM))))
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
      (start-conversation)
      (COMM::send 'test `(TELL :content (component-status :who TEST :what (TESTING ,key))))
      (test)))))


(defun enable-graphviz-display ()
  (COMM::send 'test '(request :receiver graphviz :content (enable-display))))

(defun disable-graphviz-display ()
  (COMM::send 'test '(request :receiver graphviz :content (disable-display))))


