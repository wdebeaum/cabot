
;;;;
;;;; File: test.lisp
;;;; Creator: George Ferguson
;;;; Created: Tue Jun 19 14:35:09 2012
;;;; Time-stamp: <Fri Aug 26 17:50:43 EDT 2016 jallen>
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
(defvar *sample-dialogues*
  '(
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
       "OK. Now we'll build the second row. Put a green block on the green block at the end"
       ;; OK  <<does it>>
       "Great. Now put a green one on top of it"
       ;; Shouldn't that block be red?
       "Oh yes. Make that a red block"
       ;;OK
       "Great. Now put a red block on the middle block"
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
      
  (ted01 .
  (
   "I'd like to begin by introducing the component objects we have here today."
   "This is a block. it's a red block." 
   "This is also a block"
   "it's a blue block."
   "This is a blue rectangular block."
   "This is a car"
   "it's a red car."
    "This is a blue car."
    "This is a yellow block."
    "This is a blue block.
    This is a green block.
    This is a red truck.
    This is a truck, and it's both yellow and blue.
    So one thing we can do is we can put these together according to their color.
    So what we would do is put all the items of one color in one spot, and all the items of another color in another spot.
    So we'll take the two, these are both blue blocks, we'll put them together.
    And we'll put them with the blue car.
    And here we have another blue block, which we'll put with all the blue blocks.
    And we'll put those all together here.
    Now we have a green block, it's the only green thing we have, so we'll put that in a group by itself.
    We have the red truck.
    The red block.
    And the red car.
    Now there's a little bit of black on the car, on the truck, but it's mostly red. 
    So we're going to call it red.
    Same with the car.
    There's a little bit of black on it.
    But because it's mostly red, we'll call it red.
    And, you notice that the actual color of the red is a little different, but it's mostly the same color.
    It's a lot closer to each other than they are to any of the other colors.
    It's the same with the blue, there's a little bit of white in this blue block, but it's mostly blue.
    And even though this is a darker blue than this blue, we're going to keep them together.
    So we've got all the green items here.
    All the red items here.
    This is a yellow block.
    We'll put that over here.
    And now this big truck is both yellow and blue, so we'll put it in between the yellow and the blue sections.
    So that the blue kind of goes with the blue.
    And the yellow kind of goes with the yellow.
    So you could think of this as being a yellow thing.
    Or you could think of it as being a blue thing.
    So that's organizing things according to color.
    Now the second thing we're going to do is we're going to organize these things according to shape.
    And the shape is the type of thing they are.
    So what we'll notice is that this, for instance is a square, or a cube.
    And it's got all the sides are the same size.
    This item is exactly the same size.
    They match almost perfectly.
    They're of different colors, but their shape is the same.
    So we'll put these in a section by themselves.
    Now in the same way, we could see that these two items -
    Even though they're different colors, they match according to their shape.
    They're the same shape, as you can see.
    So they go together, since they're the same shape.
    So there's a cube that needed a bigger rectangle.
    In the same way, we would notice that these two rectangular shaped objects are the same shape.
    So we'll put them here according to shape.
    So we have three groups.
    One of the cubes, one of the big rectangles, and one of the small rectangles.
    Now what you'll notice is these two cars
    Now they're not simple, they're both shaped like cars.
    Even though they're different colors, they're exactly the same shape.
    It's like they were made by the same thing almost.
    So we'll put those together even though there's a blue and a red one.
    They go together.
    Now these two items are not exactly the same shape, but they're very close to the same shape.
    They're different sizes, and of course they're different colors, but they're both truck shapes, we might call them.
    So this is a dump truck shape, and this is a dump truck shape.
    The big parts of the shape are the same.
    Now there are little details, little small parts that are different.
    But for the most part, people will think these are the same shape.
    So that's organizing things by shape.
    Now we want to do a third thing.
    We want to organize them by size.
    And what we'll do is, so size of course is smallest to biggest.
    So we'll start with the smallest items.
    And I guess we'd say that these items are pretty small.
    They're about the same total size.
    So they'll go together still.
    Things that are exactly the same shape, will be generally the same size if they're exactly the same shape.
    Then we'll say these cars might be, maybe they're a little bigger than these blocks.
    But they're about the same size.
    But they're a little bigger so we'll make them into a separate group.
    Now these blocks are even a little bigger than the cars, so they'll make another group of size.
    Now what we might say is that even these three things, even though this one is a truck, these are blocks of course.
    They're the same shape but different colors.
    But they're kind of the same size as this truck.
    The truck is probably a little bigger, but maybe we'll say these three things are kind of our big items.
    So we'll say that these are all in one group together with shape.
    You might make this one even bigger because it is a lot larger.
    So you could keep this truck in a group by itself.
    But they're pretty close.
    Sometimes that's how it is.
    Sometimes you have exact things, but sometimes it's different.
    Now this one, we have one item left which we have to figure out.
    It's probably about, it's somewhere in between the cars and these rectangles.
    So we'll make it into a separate size.
    It might be able to go into either of the groups.
    You might say well, this is about the same size as the cars.
    You might say, oh, it's about the same size as the rectangles.
    But it's not quite either one.
    So we'll say that there are one, two, three, four, five, six different sizes here.
    These are the smallest.
    The next smallest.
    A little bigger.
    Even bigger.
    Larger.
    And this is our largest item.
    So that's how we would organize things according to size."
   )
  )
(ted01-2 .
  (
   "I'd like to begin by introducing the component objects we have here today."
    "This is a block, it's a red block."
    "This is also a block, it's a blue block."
    "This is a blue rectangular block."
    "This is a car, it's a red car."
    "This is a blue car."
    "This is a yellow block."
    "This is a blue block."
    "This is a green block."
    "This is a red truck."
    "This is a truck, and it's both yellow and blue."
    "So one thing we can do is we can put these together according to their color."
    "So what we would do is put all the items of one color in one spot, and all the items of another color in another spot."
    "So we'll take the two, these are both blue blocks, we'll put them together."
    "And we'll put them with the blue car."
    "And here we have another blue block, which we'll put with all the blue blocks."
    "And we'll put those all together here."
    "Now we have a green block, it's the only green thing we have, so we'll put that in a group by itself."
    "We have the red truck."
    "The red block."
    "And the red car."
    "Now there's a little bit of black on the car, on the truck, but it's mostly red."
    "So we're going to call it red."
    "Same with the car."
    "There's a little bit of black on it."
    "But because it's mostly red, we'll call it red."
    "And, you notice that the actual color of the red is a little different, but it's mostly the same color."
    "It's a lot closer to each other than they are to any of the other colors."
    "It's the same with the blue, there's a little bit of white in this blue block, but it's mostly blue."
    "And even though this is a darker blue than this blue, we're going to keep them together."
    "So we've got all the green items here."
    "All the red items here."
    "This is a yellow block."
    "We'll put that over here."
    "And now this big truck is both yellow and blue, so we'll put it in between the yellow and the blue sections."
    "So that the blue kind of goes with the blue."
    "And the yellow kind of goes with the yellow."
    "So you could think of this as being a yellow thing."
    "Or you could think of it as being a blue thing."
    "So that's organizing things according to color."
    "Now the second thing we're going to do is we're going to organize these things according to shape."
    "And the shape is the type of thing they are."
    "So what we'll notice is that this, for instance is a square, or a cube."
    "And it's got all the sides are the same size."
    "This item is exactly the same size."
    "They match almost perfectly."
    "They're of different colors, but their shape is the same."
    "So we'll put these in a section by themselves."
    "Now in the same way, we could see that these two items."
    "Even though they're different colors, they match according to their shape."
    "They're the same shape, as you can see."
    "So they go together, since they're the same shape."
    "So there's a cube that needed a bigger rectangle."
    "In the same way, we would notice that these two rectangular shaped objects are the same shape."
    "So we'll put them here according to shape."
    "So we have three groups."
    "One of the cubes."
    "One of the big rectangles."
    "And one of the small rectangles."
    "Now what you'll notice is these two cars."
    "Now they're not simple, they're both shaped like cars."
    "Even though they're different colors, they're exactly the same shape."
    "It's like they were made by the same thing almost."
    "So we'll put those together even though there's a blue and a red one."
    "They go together."
    "Now these two items are not exactly the same shape, but they're very close to the same shape."
    "They're different sizes, and of course they're different colors, but they're both truck shapes, we might call them."
    "So this is a dump truck shape, and this is a dump truck shape."
    "The big parts of the shape are the same."
    "Now there are little details, little small parts that are different."
    "But for the most part, people will think these are the same shape."
    "So that's organizing things by shape."
    "Now we want to do a third thing."
    "We want to organize them by size."
    "And what we'll do is, so size of course is smallest to biggest."
    "So we'll start with the smallest items."
    "And I guess we'd say that these items are pretty small."
    "They're about the same total size."
    "So they'll go together still."
    "Things that are exactly the same shape, will be generally the same size if they're exactly the same shape."
    "Then we'll say these cars might be, maybe they're a little bigger than these blocks."
    "But they're about the same size."
    "But they're a little bigger so we'll make them into a separate group."
    "Now these blocks are even a little bigger than the cars, so they'll make another group of size."
    "Now what we might say is that even these three things" 
    "Even though this one is a truck" 
    "These are blocks of course."
    "They're the same shape but different colors."
    "But they're kind of the same size as this truck."
    "The truck is probably a little bigger, but maybe we'll say these three things are kind of our big items."
    "So we'll say that these are all in one group together with shape."
    "You might make this one even bigger because it is a lot larger."
    "So you could keep this truck in a group by itself."
    "But they're pretty close."
    "Sometimes that's how it is."
    "Sometimes you have exact things, but sometimes it's different."
    "Now this one, we have one item left which we have to figure out."
    "It's probably about, it's somewhere in between the cars and these rectangles."
    "So we'll make it into a separate size."
    "It might be able to go into either of the groups."
    "You might say well, this is about the same size as the cars."
    "You might say, oh, it's about the same size as the rectangles."
    "But it's not quite either one."
    "So we'll say that there are one, two, three, four, five, six different sizes here."
    "These are the smallest."
    "The next smallest."
    "A little bigger."
    "Even bigger."
    "Larger."
    "And this is our largest item."
    "So that's how we would organize things according to size."
   )
  )

(NIST-3 .
(;1345649236.86 1345649239.39 
"This is a rectangle with three holes."
;1345649247.02 1345649250.24 
"This is a blue rectangular"
;1345649250.98 1345649251.56 
"What did you call it?"
;1345649252.56 1345649252.90 
"Block."
;1345649254.66 1345649255.77 
"Blue rectangular block."
;1345649262.72 1345649263.76 
"This is a red cube."
;1345649268.19 1345649269.30 
"This is a blue cube."
;1345649272.99 1345649274.07 
"This is a green cube."
;1345649282.56 1345649283.97 
"This is a screwdriver."
;1345649291.92 1345649294.52 
"This is a yellow square with a hole in it."
;1345649299.86 1345649302.17 
"This is a red square with a hole in it."
;1345649310.71 1345649313.04 
"This is a red screw."
;1345649324.83 1345649327.12 
"This is an orange screwdriver."
;1345649342.81 1345649344.00 
"This is a blue square."
;1345649344.55 1345649345.90 
"A blue screw."
;1345649348.29 1345649349.17 
"It's a blue screw."
;1345649352.20 1345649353.69 
"This is a green screw."
;1345649363.19 1345649365.96 
"This is a blue hexagon with a hole in it."
;1345649375.68 1345649378.49 
"This is a black wrench."
;1345649390.56 1345649392.00 
"This is a blue wrench."
;1345649397.39 1345649397.95 
"I'll stop there."
;1345649402.32 1345649404.14 
"Do I keep going to ask any questions?"
;1345649404.46 1345649404.66 
"Ok."
;1345649405.48 1345649405.66 
"Alright."
;1345649416.91 1345649417.78 
"What is this?"
;1345649427.00 1345649427.66 
"What is this?"
;1345649437.03 1345649437.76 
"What is this?"
;1345649447.74 1345649448.33 
"What is this?"
;1345649455.94 1345649456.40 
"What is this?"
;1345649463.08 1345649463.59 
"What is this?"
;1345649472.25 1345649472.78 
"What is this?"
;1345649480.43 1345649480.97 
"What is this?"
;1345649493.65 1345649494.30 
"What is this?"
;1345649498.14 1345649498.51 
"Ok."
))

(NIST-4 .
(
;1345649755.26 1345649755.59 
"Okay."
;1345649758.87 1345649760.87 
"This is a green triangular block."
;1345649763.46 1345649765.06 
"This is a yellow triangular block."
;1345649767.39 1345649769.10 
"This is a yellow tennis ball."
;1345649771.02 1345649772.47 
"This is a green tennis ball."
;1345649776.16 1345649778.82 
"This is a blue square block."
;1345649781.48 1345649783.13 
"This is a red square block."
;1345649785.76 1345649786.49 
"This is a yellow sponge."
;1345649788.77 1345649789.10 
"I'm finished."
;1345649795.03 1345649795.40 
"Okay."
;1345649800.49 1345649801.19 
"What is this?"
;1345649803.60 1345649804.08 
"What is this?"
;1345649813.34 1345649813.89 
"What is this?"
;1345649815.67 1345649816.13 
"What is this?"
;1345649825.04 1345649825.45 
"What is this?"
;1345649827.49 1345649827.88 
"What is this?"
;1345649829.87 1345649830.19 
"What is this?"
;1345649835.48 1345649835.84 
"What is this?"
;1345649837.08 1345649837.52 
"What is this?"
;1345649838.93 1345649839.36 
"What is this?"
;1345649846.76 1345649847.30 
"What is this?"
;1345649848.75 1345649849.19 
"What is this?"
;1345649850.55 1345649851.17 
"And what is this?"
))

(ted01-commaless .
  (
   "I'd like to begin by introducing the component objects we have here today."
    "This is a block it's a red block."
    "This is also a block it's a blue block."
    "This is a blue rectangular block."
    "This is a car it's a red car."
    "This is a blue car."
    "This is a yellow block."
    "This is a blue block."
    "This is a green block."
    "This is a red truck."
    "This is a truck and it's both yellow and blue."
    "So one thing we can do is we can put these together according to their color."
    "So what we would do is put all the items of one color in one spot and all the items of another color in another spot."
    "So we'll take the two these are both blue blocks we'll put them together."
    "And we'll put them with the blue car."
    "And here we have another blue block which we'll put with all the blue blocks."
    "And we'll put those all together here."
    "Now we have a green block it's the only green thing we have so we'll put that in a group by itself."
    "We have the red truck."
    "The red block."
    "And the red car."
    "Now there's a little bit of black on the car on the truck but it's mostly red."
    "So we're going to call it red."
    "Same with the car."
    "There's a little bit of black on it."
    "But because it's mostly red we'll call it red."
    "And you notice that the actual color of the red is a little different but it's mostly the same color."
    "It's a lot closer to each other than they are to any of the other colors."
    "It's the same with the blue there's a little bit of white in this blue block but it's mostly blue."
    "And even though this is a darker blue than this blue we're going to keep them together."
    "So we've got all the green items here."
    "All the red items here."
    "This is a yellow block."
    "We'll put that over here."
    "And now this big truck is both yellow and blue so we'll put it in between the yellow and the blue sections."
    "So that the blue kind of goes with the blue."
    "And the yellow kind of goes with the yellow."
    "So you could think of this as being a yellow thing."
    "Or you could think of it as being a blue thing."
    "So that's organizing things according to color."
    "Now the second thing we're going to do is we're going to organize these things according to shape."
    "And the shape is the type of thing they are."
    "So what we'll notice is that this for instance is a square or a cube."
    "And it's got all the sides are the same size."
    "This item is exactly the same size."
    "They match almost perfectly."
    "They're of different colors but their shape is the same."
    "So we'll put these in a section by themselves."
    "Now in the same way we could see that these two items."
    "Even though they're different colors they match according to their shape."
    "They're the same shape as you can see."
    "So they go together since they're the same shape."
    "So there's a cube that needed a bigger rectangle."
    "In the same way we would notice that these two rectangular shaped objects are the same shape."
    "So we'll put them here according to shape."
    "So we have three groups."
    "One of the cubes."
    "One of the big rectangles."
    "And one of the small rectangles."
    "Now what you'll notice is these two cars."
    "Now they're not simple they're both shaped like cars."
    "Even though they're different colors they're exactly the same shape."
    "It's like they were made by the same thing almost."
    "So we'll put those together even though there's a blue and a red one."
    "They go together."
    "Now these two items are not exactly the same shape but they're very close to the same shape."
    "They're different sizes and of course they're different colors but they're both truck shapes we might call them."
    "So this is a dump truck shape and this is a dump truck shape."
    "The big parts of the shape are the same."
    "Now there are little details little small parts that are different."
    "But for the most part people will think these are the same shape."
    "So that's organizing things by shape."
    "Now we want to do a third thing."
    "We want to organize them by size."
    "And what we'll do is so size of course is smallest to biggest."
    "So we'll start with the smallest items."
    "And I guess we'd say that these items are pretty small."
    "They're about the same total size."
    "So they'll go together still."
    "Things that are exactly the same shape will be generally the same size if they're exactly the same shape."
    "Then we'll say these cars might be maybe they're a little bigger than these blocks."
    "But they're about the same size."
    "But they're a little bigger so we'll make them into a separate group."
    "Now these blocks are even a little bigger than the cars so they'll make another group of size."
    "Now what we might say is that even these three things" 
    "Even though this one is a truck" 
    "These are blocks of course."
    "They're the same shape but different colors."
    "But they're kind of the same size as this truck."
    "The truck is probably a little bigger but maybe we'll say these three things are kind of our big items."
    "So we'll say that these are all in one group together with shape."
    "You might make this one even bigger because it is a lot larger."
    "So you could keep this truck in a group by itself."
    "But they're pretty close."
    "Sometimes that's how it is."
    "Sometimes you have exact things but sometimes it's different."
    "Now this one we have one item left which we have to figure out."
    "It's probably about it's somewhere in between the cars and these rectangles."
    "So we'll make it into a separate size."
    "It might be able to go into either of the groups."
    "You might say well this is about the same size as the cars."
    "You might say oh it's about the same size as the rectangles."
    "But it's not quite either one."
    "So we'll say that there are one two three four five six different sizes here."
    "These are the smallest."
    "The next smallest."
    "A little bigger."
    "Even bigger."
    "Larger."
    "And this is our largest item."
    "So that's how we would organize things according to size."
   )
  )
(nist-1 .
 (;1345647522.60 1345647523.02 
"I'm going to train the system."
;1345647524.34 1345647531.42 
"so this is a blue rectangular tall block."
;1345647535.61 1345647538.98 
"this is a small blue tennis ball."
;1345647541.03 1345647541.87 
"and it's a rolling tennis ball."
;1345647547.14 1345647551.25 
"this is a large green triangle."
;1345647553.85 1345647554.97 
"this is the triangle on its side."
;1345647562.03 1345647565.26 
"this is a yellow sponge."
;1345647566.39 1345647567.07 
"a rectangular sponge."
;1345647572.96 1345647578.84 
"this is a tall white cylindrical paint roller."
;1345647580.57 1345647581.00 
"paint roller."
;1345647585.78 1345647591.49 
"and this is a blue square cube."
;1345647591.97 1345647592.70 
"small cube."
;1345647595.70 1345647600.91 
"this object here the one in my hand is a green small cube."
;1345647605.78 1345647610.87 
"and this is another white paint roller that's still in the bag."
;1345647617.39 1345647619.52 
"here's another yellow sponge."
;1345647620.08 1345647620.91 
"rectangular sponge."
;1345647622.17 1345647623.06 
"soft rectangular sponge."
;1345647627.01 1345647629.65 
"and here is another paint roller."
;1345647630.32 1345647635.01 
"it's just like the other paint roller except this one is orange but it's also tall and cylindrical."
;1345647639.23 1345647650.13 
"and here is another yellow triangular relatively large shape very similar to the green shape."
;1345647653.87 1345647659.30 
"and the last object here is a bouncing green tennis ball."
;1345647661.30 1345647661.99 
"spherical tennis ball."
;1345647663.96 1345647666.06 
"so if we sort these by color."
;1345647667.91 1345647674.05 
"here's the yellow triangular shape and another yellow sponge."
;1345647675.94 1345647677.09 
"and here's another yellow sponge."
;1345647679.61 1345647682.69 
"and this one isn't quite yellow but it's almost yellow."
;1345647683.27 1345647686.48 
"it's kind of an orange shape."
;1345647687.03 1345647688.93 
"so here are the shapes that are yellow or close to yellow."
;1345647691.34 1345647697.61 
"and the green shapes are this green triangle."
;1345647699.40 1345647702.33 
"and the green spherical tennis ball."
;1345647704.12 1345647705.78 
"and a green cube."
;1345647707.56 1345647714.04 
"and the blue shapes are a blue tall rectangular object."
;1345647715.18 1345647715.89 
"and a blue cube."
;1345647718.04 1345647719.81 
"and a blue rolling tennis ball."
;1345647722.11 1345647723.42 
"and then we have two white objects."
;1345647724.35 1345647725.26 
"and they're both paint rollers."
;1345647726.97 1345647729.04 
"one that's in the bag and one that's not in the bag."
;1345647730.49 1345647733.90 
"they're both cylindrical and tall when they're standing on their side."
;1345647737.00 1345647740.34 
"and lastly we'll sort by size."
;1345647745.85 1345647746.92 
"so here we have a tall blue rectangular object."
;1345647749.83 1345647754.78 
"and two white paint rollers."
;1345647756.06 1345647756.99 
"cylindrical tall paint rollers."
;1345647759.21 1345647762.49 
"and then the smaller objects are the green tennis ball."
;1345647763.81 1345647765.29 
"and the blue tennis ball."
;1345647767.50 1345647768.81 
"and the blue cube."
;1345647771.24 1345647773.20 
"and the green square cube."
;1345647774.42 1345647775.54 
"oh and here's another tall object."
;1345647776.20 1345647778.72 
"here's another tall paint roller."
;1345647779.03 1345647779.41 
"this one is orange."
;1345647782.56 1345647788.68 
"and then we have for small objects we have the small sponge the yellow sponge and then a second yellow sponge."
;1345647789.85 1345647794.96 
"and then we have two triangular shapes one yellow one green."
;1345647795.89 1345647796.56 
"and these are kind of medium size."
))
(NIST-2 . 
 (;1345648489.99 1345648490.46 
"Ok"
;1345648490.85 1345648491.18 
"Good"
;1345648492.22 1345648492.52 
"Good morning"
;1345648493.27 1345648494.22 
"I'm here to talk about some objects today."
;1345648495.90 1345648496.59 
"This is a plane."
;1345648498.50 1345648499.04 
"It's flat."
;1345648503.43 1345648504.71 
"This is a triangular block."
;1345648506.08 1345648507.47 
"The color of the block is green."
;1345648512.24 1345648514.11 
"This is a large green disk."
;1345648515.47 1345648517.91 
"This disk has a triangular hole."
;1345648522.75 1345648524.54 
"This is a small green disk."
;1345648525.98 1345648528.18 
"This green disk also has a triangular hole."
;1345648535.21 1345648536.71 
"This is a blue block."
;1345648543.23 1345648544.34 
"This is a blue disk."
;1345648545.55 1345648547.89 
"This disk has a square hole."
;1345648551.56 1345648553.09 
"We are just gonna move this over here."
;1345648555.88 1345648556.69 
"This is a ball."
;1345648557.22 1345648558.48 
"The color of the ball is white."
;1345648560.39 1345648561.60 
"This is also a baseball."
;1345648565.20 1345648565.90 
"This is a blue ball."
;1345648566.79 1345648567.44 
"This is a tennis ball."
;1345648571.73 1345648574.56 
"This is a purple ball also a tennis ball."
;1345648577.91 1345648578.71 
"This is a red ball."
;1345648582.92 1345648584.28 
"This is also a small ball."
;1345648586.65 1345648587.46 
"This is a large ball."
;1345648588.86 1345648591.99 
"This is a white ball also called a soccer ball."
;1345648600.45 1345648602.95 
"This object is a black cube."
;1345648605.45 1345648606.75 
"This object is a red cube."
;1345648608.44 1345648609.51 
"This is a green cube."
;1345648616.55 1345648617.54 
"This is a stack."
;1345648625.99 1345648627.78 
"This is a green triangular block."
;1345648631.85 1345648633.62 
"This is a yellow triangular block."
;1345648638.03 1345648639.38 
"This is a square block."
;1345648641.18 1345648641.72 
"Two colors."
;1345648652.07 1345648652.85 
"This is a container."
;1345648653.99 1345648655.07 
"This is a green container."
;1345648656.53 1345648657.42 
"This is a red container."
;1345648658.84 1345648659.85 
"This is a black container."
;1345648665.46 1345648666.89 
"This is a stack of containers."
;1345648674.14 1345648675.35 
"This is a lid."
;1345648675.35 1345648676.57 
"The lid is square."
;1345648685.99 1345648687.03 
"This is a hammer."
;1345648693.83 1345648695.50 
"This hammer is yellow and blue."
;1345648698.66 1345648699.82 
"This is a yellow hammer."
;1345648704.76 1345648705.92 
"This is a red hammer."
;1345648711.24 1345648712.05 
"This is a sphere."
;1345648713.35 1345648714.39 
"This is not a ball."
;1345648723.40 1345648726.36 
"And can you toss me the different color hammers please?"
;1345648731.89 1345648732.38 
"What else should we do?"
;1345648747.53 1345648747.99 
"Are you having fun yet?"
;1345648752.18 1345648752.59 
"And I think I am good."
;1345648756.74 1345648757.15 
"I like that."
;1345648758.65 1345648759.05 
"I'll ask questions."
;1345648763.17 1345648763.51 
"Yeah yeah."
;1345648766.36 1345648767.19 
"Don't worry I won't go nuts."
;1345648774.09 1345648775.58 
"Hold on how long was that training session Ian?"
;1345648780.10 1345648780.48 
"Alright questions."
;1345648785.22 1345648785.67 
"What is this?"
;1345648791.26 1345648792.18 
"Can you tell me what this is?"
;1345648796.97 1345648797.47 
"What about this?"
;1345648803.78 1345648804.33 
"What is this?"
;1345648811.48 1345648812.11 
"What is this?"
;1345648818.79 1345648819.34 
"What is this?"
;1345648824.85 1345648825.68 
"Can you tell me what this is?"
;1345648831.99 1345648833.19 
"What is this?"
;1345648837.50 1345648837.95 
"What about this?"
;1345648843.60 1345648844.48 
"Can you tell me what this is?"
;1345648849.66 1345648850.39 
"What is this?"
;1345648856.77 1345648857.43 
"What is this?"
;1345648860.14 1345648861.36 
"Can you tell me what this is now?"
;1345648864.83 1345648865.33 
"What is this?"
;1345648869.88 1345648870.57 
"What about this?"
;1345648875.11 1345648875.65 
"What is this?"
;1345648880.88 1345648881.36 
"What is this?"
;1345648883.86 1345648884.57 
"Do you know what this is?"
;1345648889.55 1345648889.93 
"What is this?"
;1345648915.03 1345648915.49 
"Can you tell me what this is?"
;1345648917.69 1345648918.04 
"What about this?"
;1345648920.25 1345648920.76 
"What is this?"
;1345648926.07 1345648926.67 
"Do you know what this is?"
;1345648928.68 1345648929.57 
"Can you tell me what this is now?"
;1345648935.67 1345648936.19 
"What is this?"
;1345648939.61 1345648940.00 
"Do you now know what it is?"
;1345648947.73 1345648949.22 
"Now can you tell me what this is?"
;1345648954.55 1345648954.95 
"I think we are good."
  ))
 (unknown-words .
     ("This is a block. 
       This is yellow.
       This is a yellow block.
       He flicks the block.
       He flicks the door.
       He flicks the yellow block."  
      
      ))
    (unknown-words-2 .
     ("This is a block." 
       "This is yellow."
       "This is a yellow block."
       "He flicks the block."
       "He flicks the door."
       "He flicks the yellow block."
       "This is a block it's a yellow block."
       "Describe this one."
       "Flick the yellow block."
       "Describe this."
       "Describe one."
      ))
   (elif-blocks .
     ("Hi everyone. 
      My task today is to separate these blocks, this group of blocks, into three different categories according to their color.
      First I begin with the yellow objects.
      This is a yellow square block.
      A yellow triangular block.
      And a yellow arch-shaped block. 
      Okay, next, red.
      This is a red square shaped block. 
      A red rectangular block. 
      A red triangular block. 
      And a red, again a rectangular block, but this is short.
      And this is long. 
      And a red crescent shaped block. 
      Next, blue. 
      Blue rectangular block. 
      Blue square block. 
      Blue arc-shaped block. 
      And blue triangular block. 
      And the rest are the green blocks. 
      Again square, triangular, and two rectangular blocks. 
      And now I'm going to show you objects in terms of their shape. 
      So let's find triangular blocks.  
      Triangular blue block, triangular yellow block, triangular green block, and triangular red block. 
      We have four blocks, all in different colors. 
      Now let's find the square shaped blocks. 
      Yellow square block, green square block, blue square block, and red square block. 
      Let's put them in the same order as the triangular blocks. 
      Okay, we have only two arc shaped blocks. 
      Here is the blue one. 
      Here is the yellow one. 
      And maybe we can just put them next to the other ones. 
      Okay. 
      The rest, we have lots of rectangular blocks in different lengths.  
      Two rectangular blocks, red and blue. 
      These are both long. 
      And we have two short green rectangular blocks. 
      Finally we have one red short rectangular block. 
      And, the only crescent shaped block we have. 
      Where should I put this? 
      Maybe, here."
      ))
(elif-blocks-nopunc .
     ("Hi everyone"
      "My task today is to separate these blocks this group of blocks into three different categories according to their color"
      "First I begin with the yellow objects"
      "This is a yellow square block"
      "A yellow triangular block"
      "And a yellow arch-shaped block"
      "Okay next red"
      "This is a red square shaped block"
      "A red rectangular block"
      "A red triangular block "
      "And a red again a rectangular block but this is short"
      "And this is long"
      "And a red crescent shaped block"
      "Next blue"
      "Blue rectangular block"
      "Blue square block"
      "Blue arc-shaped block" 
      "And blue triangular block"
      "And the rest are the green blocks"
      "Again square triangular and two rectangular blocks"
      "And now I'm going to show you objects in terms of their shape"
      "So let's find triangular blocks"
      "Triangular blue block triangular yellow block triangular green block and triangular red block"
      "We have four blocks all in different colors"
      "Now let's find the square shaped blocks"
      "Yellow square block green square block blue square block and red square block"
      "Let's put them in the same order as the triangular blocks"
      "Okay we have only two arc shaped blocks"
      "Here is the blue one"
      "Here is the yellow one"
      "And maybe we can just put them next to the other ones"
      "Okay"
      "The rest we have lots of rectangular blocks in different lengths"
      "Two rectangular blocks red and blue"
      "These are both long"
      "And we have two short green rectangular blocks"
      "Finally we have one red short rectangular block" 
      "And the only crescent shaped block we have" 
      "Where should I put this"
      "Maybe here"
      ))
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
(setf *test-dialog*
  (cdr (assoc 'mark-demo *sample-dialogues*)))

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
      (test)))))


(defun enable-graphviz-display ()
  (COMM::send 'test '(request :receiver graphviz :content (enable-display))))

(defun disable-graphviz-display ()
  (COMM::send 'test '(request :receiver graphviz :content (disable-display))))

;; This function probably belongs in core/test.lisp
(defun test-all ()
  "Invoke TEST on all utterances of *TEST-DIALOG* in order.
This function does not pause between utterance, wait for results to be
finished, or any other smart thing. It simply pumps the messages in using
TEST."
  (loop for x in *test-dialog*
     do (test x)
       ;; add a wait for procesing
       ;(loop for i from 1 to 2
	;  do ;(format t ".")
	 ;   (sleep 1))
       ))

;; Ditto
(defun test-all-of (key)
  "Set *TEST-DIALOG* to the dialog identified by KEY on *SAMPLE-DIALOGUES*,
then invoke TEST-ALL to test all its utterances."
  (setf *test-dialog* (cdr (assoc key *sample-dialogues*)))
  (test-all))
