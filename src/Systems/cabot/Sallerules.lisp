;  Basic Dialogue Agent State Management

(in-package :dagent)

;; Simple system with the one basic user -- the demonstrator
(reset-states)

(setq *active-users* '("desktop"))

(define-user "desktop"
 (make-user 
  :channel-id 'desktop
  :name "demonstrator"
  ))

;; Here is the dialogue manager for handling explicit demonstration sequences
;; The state START-EXPLICIT-DEMONSTRATION defines the triggers to start the segment
;; and EXPLCIT-DEMONSTRATION-SEGMENT is the single state that performed elipsis
;;   management for subsequent demonstrations within the segement. We stay in this
;;  state until an explicit closing acknowledgement is made -- and  can switch to
;;  new defaults with additional explicit utterances

(add-state 'start-explicit-demonstration
	   (state :action NIL
		  ;;:preprocessing-ids '(demonstrations)
		  :transitions 
		  (list
		  
		   (transition 
		    :pattern
		     ;; here is the block
		     '((ONT::F ?ev ONT::BE :neutral ?!theme :neutral1 ?!cotheme)
		       ((? PP ONT::PRO ONT::PRO-SET) ?!theme ?themetype :proform (? xx ont::this ont::these ont::those ont::that))
		       ((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET)
			?!cotheme (? cothemetype ont::referential-sem))
		       -start-explicit-demonstration2a>
		       (record class ?cothemetype)
		       (record props nil)
		       (call (construct-and-send-msg (demonstrated :id ?!cotheme)))
		       )
		     :destination 'explicit-demonstration-segment
		     :trigger t)
		   
		   (transition 
		    :pattern
		     ;; the block is here
		     '((ONT::F ?ev ONT::BE :neutral ?!cotheme :neutral1 ?!theme)
		       ((? PP ONT::PRO ONT::PRO-SET) ?!theme ?themetype :proform (? xx ont::this ont::these ont::those ont::that))
		       ((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET)
			?!cotheme (? cothemetype ont::referential-sem))
		       -start-explicit-demonstration2b>
		       (record class ?cothemetype)
		       (record props nil)
		       (call (construct-and-send-msg (demonstrated :id ?!cotheme)))
		       )
		     :destination 'explicit-demonstration-segment
		     :trigger t)

		   (transition :pattern ;; we have a block
			 '((ONT::F ?ev (:* ?ontype w::have) :neutral1 ?!theme :neutral ?!affected)
			   ((? PP ONT::PRO ONT::PRO-SET) ?!affected ?afftype :proform (? x ont::we ont::you))
			   ((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET)  ?!theme ?themetype)
			   -start-explicit-demonstration6>
			   (call (construct-and-send-msg 
				  (demonstrated :id ?!theme))))
			 :destination 'explicit-demonstration-segment
			 :trigger t)

		   ;; the block is beside the cube
		    (transition :pattern
			  '((ONT::F ?ev ONT::HAVE-PROPERTY :neutral ?!theme :formal ?!property)
			    ((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET)
			     ?!theme ?themetype )
			    (ONT::F ?!property ?proptype :of ?!theme :val ?!val)
			    ((? reln1 ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET)
			     ?!val ?valtype)
			    -explicit-relation-new1>
			     (call (construct-and-send-msg 
				    (relate :id ?!theme :relation ?proptype :ground ?!val))))
			  :destination 'explicit-demonstration-segment
			  :trigger t)

		    ;; there is a block beside the cube
		    (transition :pattern
			  '((ONT::F ?ev ONT::EXISTS :neutral ?!theme :mod ?!property)
			    ((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET)
			     ?!theme ?themetype )
			    (ONT::F ?!property (? proptype ONT::ABSTRACT-OBJECT) :of ?ev :val ?!val)
			    ((? reln1 ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET)
			     ?!val ?valtype)
			    -there-explicit-relation-new1>
			     (call (construct-and-send-msg 
				    (relate :id ?!theme :relation ?proptype :ground ?!val))))
			  :destination 'explicit-demonstration-segment
			  :trigger t)

		     ;; there is a block near the cube    // sometimes we get a :atloc rather than a :mod
		    (transition :pattern
			  '((ONT::F ?ev ONT::EXISTS :neutral ?!theme :at-loc ?!property)
			    ((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET)
			     ?!theme ?themetype )
			    (ONT::F ?!property (? proptype ONT::ABSTRACT-OBJECT) :of ?ev :val ?!val)
			    ((? reln1 ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET)
			     ?!val ?valtype)
			    -there-explicit-relation-new2>
			     (call (construct-and-send-msg 
				    (relate :id ?!theme :relation ?proptype :ground ?!val))))
			  :destination 'explicit-demonstration-segment
			  :trigger t)


		    (transition :pattern ;; this is a block
				'((ONT::F ?ev  ONT::EXISTS :neutral ?!theme)
				  ((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET)  ?!theme ?themetype)
				  -start-explicit-demonstration9>
				  (call (construct-and-send-msg 
					 (demonstrated :id ?!theme))))
				;;(IdentifyObject ?!theme ?themetype))
				:destination 'explicit-demonstration-segment
				:trigger t)

		    (transition :pattern ;; this block is red [iperera]
			  '((ONT::F ?ev ONT::HAVE-PROPERTY :neutral ?!theme :formal ?!cotheme)
			    ((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET)  ?!theme ?themetype)
			    (ont::F ?!cotheme ?cothemetype :of ?!theme)
			    -start-explicit-demonstration10>
			    (record class ?themetype)
			    (record props (?cothemetype))
			    (call (construct-and-send-msg
				   (demonstrated :id ?!theme ))))
			    :destination 'explicit-demonstration-segment
			    :trigger t)
		   
		   )))

;; This handles the ellipsis often seen in a series of demonstrations
(add-state 'explicit-demonstration-segment
	   (state  ;;:preprocessing-ids '(demonstrations attribute)  ;; mention
		   :implicit-confirm t
		   :transitions
		 
		  (list
		 #||   ;; another explicit demonstration,  "this is red box"
		   (transition
		    :pattern
		    '((IdentifyObject ?v ?class :properties ?props)
		      -continue-explicit-demonstration>
		      (record class ?class)
		      (record props ?!props)
		      (call (construct-and-send-msg (demonstrated :id ?v))))
		    :destination 'explicit-demonstration-segment)||#

		   ;; simple mention   --- e.g., a red box
		  (transition
		    :pattern
		    '((ont::speechact ?!xx ONT::SA_IDENTIFY :content ?!term)
		      -continue-explicit-demonstration1>
		      (call (construct-and-send-msg (demonstrated :id ?!term))))
		    :destination 'explicit-demonstration-segment)
		 
		  ;; attributes of identified object e.g., this is red
		  (transition
		    :pattern
		    '((ONT::F ?ev ONT::HAVE-PROPERTY :neutral ?!theme :formal ?!property)
		      ((? PP ONT::PRO ONT::PRO-SET) ?!theme ?themetype :proform (? x ont::it ont::this ont::these ont::they))
		      (ONT::F ?!property (? proptype ONT::ABSTRACT-OBJECT))
		      -continue-with-attribute>
		      (call (construct-and-send-msg 
			     (described :id ?!theme :properties (?proptype)))))
		    :destination 'explicit-demonstration-segment)
		  
		  ;; here are some signal that close the current demonstration sequence
		  ;;     and we clear the stored variables used for ellipsis
		  ;; e.g., good

		  (transition  
		   :pattern
		   '((ont::speechact ?!x ont::evaluate)
		     -close-demonstration1>
		     (record class nil)
		     (record properties nil)
		     )
		   :destination 'segmentend
		  )
		  ;; e.g., next
		  (transition
		   :pattern
		   '((ont::speechact ?!x ont::manage-conversation :signal ?!signal)
		     -close-demonstration2>
		     (record class nil)
		     (record properties nil)
		     )
		   :destination 'segmentend
		  )
		  )))


(add-preprocessing 'demonstrations
  (list
   (rule :pattern 
	 ;; this is a large block
	 '((ONT::F ?ev ONT::BE :neutral ?!theme :neutral1 ?!cotheme)
	   ((? PP ONT::PRO ONT::PRO-SET) ?!theme ?themetype :proform (? xx ont::this ont::these ont::those))
	   ((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET)  ?!cotheme ?cothemetype :mod ?!mod)
	   (ont::F ?!mod (? modtype ont::property-val ont::modifier))
	   -explicit-demonstration1>
	   (IdentifyObject ?!cotheme ?cothemetype :properties (?modtype))))
   (rule :pattern
	 ;; this is a block
	 '((ONT::F ?ev ONT::BE :neutral ?!theme :neutral1 ?!cotheme)
	   ((? PP ONT::PRO ONT::PRO-SET) ?!theme ?themetype :proform (? xx ont::this ont::these ont::those))
	   ((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET)  ?!cotheme ?cothemetype)
	  -explicit-demonstration2>
	  (IdentifyObject ?!cotheme ?cothemetype)))
	 
   (rule :pattern ;; this is a large red block
	 '((ONT::F ?ev ONT::BE :neutral ?!theme :neutral1 ?!cotheme)
	   ((? pp ONT::PRO ONT::PRO-SET) ?!theme ?themetype :proform (? xx ont::this ont::these ont::those))
	   ((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET)  ?!cotheme ?cothemetype :mod ?!mod1 :mod1 ?!mod2)
	  (ont::F ?!mod1 (? modtype1 ont::property-val ont::modifier))
	  (ont::F ?!mod2 (? modtype2 ont::property-val ont::modifier))
	   -explicit-demonstration3>
	   (IdentifyObject ?!cotheme ?cothemetype :properties (?modtype1 ?modtype2))))

   (rule :pattern ;; we have a large red block
	 '((ONT::F ?ev (:* ?ontype w::have) :neutral1 ?!theme :neutral ?!affected)
	   ((? PP ONT::PRO ONT::PRO-SET) ?!affected ?aftype :proform (? x ont::we ont::you ont::i))
	   ((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) 
	    ?!theme ?themetype :mod ?mod1 :mod1 ?!mod2)
	   (ont::F ?!mod1 (? modtype1 ont::property-val ont::modifier))
	   (ont::F ?!mod2 (? modtype2 ont::property-val ont::modifier))
	   -explicit-demonstration4>
	   (IdentifyObject ?!theme ?themetype :properties (?modtype1 ?modtype2))))

   (rule :pattern ;;  you have a large block
	 '((ONT::F ?ev (:* ?ontype w::have) :neutral1 ?!theme :neutral ?!affected)
	   ((? PP ONT::PRO ONT::PRO-SET) ?!affected ?aftype :proform (? x ont::we ont::you ont::i))
	   ((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET)  ?!theme ?themetype :mods (?!mod1))
	   (ont::F ?!mod1 (? modtype1  ont::property-val ont::modifier))
	   -explicit-demonstration5>
	    (IdentifyObject ?!theme ?themetype :properties (?modtype1)))
	 )
   

   (rule :pattern ;; we have a block
	 '((ONT::F ?ev (:* ?ontype w::have) :neutral1 ?!theme :neutral ?!affected)
	   ((? PP ONT::PRO ONT::PRO-SET) ?!affected ?afftype :proform (? x ont::we ont::you))
	   ((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET)  ?!theme ?themetype)
	   -explicit-demonstration6>
	    (IdentifyObject ?!theme ?themetype))
	 )
   

   (rule :pattern ;; here's a large red block
	 '((ONT::F ?ev ONT::EXISTS :neutral ?!theme)
	   ((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET)  ?!theme ?themetype :mod ?!mod1 :mod1 ?!mod2)
	   (ont::F ?!mod1 ?modtype1)
	   (ont::F ?!mod2 ?modtype2)
	   -explicit-demonstration7>
	   (record class ?themetype)
	   (record props (?modtype1 ?modtype2))
	   (IdentifyObject ?!theme ?themetype :properties (?modtype1 ?modtype2)))
	 )

   (rule :pattern ;; this is a large block
	 '((ONT::F ?ev  ONT::EXISTS :neutral ?!theme)
	   ((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET)  ?!theme ?themetype :mods (?!mod1))
	   (ont::F ?!mod1 ?modtype1)
	   -explicit-demonstration8>
	   (record class ?themetype)
	   (record props (?modtype1 ?modtype2))
	   (IdentifyObject ?!theme ?themetype :properties (?modtype1)))
	 )

   (rule :pattern ;; this is a block
	 '((ONT::F ?ev  ONT::EXISTS :neutral ?!theme)
	   ((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET)  ?!theme ?themetype)
	   -explicit-demonstration9>
	   (IdentifyObject ?!theme ?themetype))
	 )

   (rule :pattern ;; this block is red [iperera]
	 '((ONT::F ?ev ONT::HAVE-PROPERTY :neutral ?!theme :formal ?!cotheme)
	   ((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET)  ?!theme ?themetype)
	   (ont::F ?!cotheme ?cothemetype)
	   -explicit-demonstration10>
	   (record class ?themetype)
	   (record props (?cothemetype))
	   (IdentifyObject ?!theme ?themetype :properties (?cothemetype))))
   ))
   


(add-preprocessing 'attribute
		   (list
		    ;;  it is red
		    (rule :pattern
			  '((ONT::F ?ev ONT::HAVE-PROPERTY :neutral ?!theme :formal ?!property)
			    ((? PP ONT::PRO ONT::PRO-SET) ?!theme ?themetype :proform (? x ont::it ont::this ont::these))
			    (ONT::F ?!property (? proptype ONT::ABSTRACT-OBJECT))
			    -explicit-attribute1>
			    (Described ?!theme ?themetype :properties (?proptype))))
		    ;; its a red block
		    (rule :pattern
			  '((ONT::F ?ev ONT::BE :neutral ?!theme :neutral1 ?!cotheme)
			    ((? PP ONT::PRO ONT::PRO-SET) ?!theme ?themetype :proform (? x ont::it))
			    ((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM) 
			     ?!cotheme ?cothemetype :mod ?mod)
			    (ont::F ?mod ?modtype)
			    -explicit-attribute2>
			    (Described ?!theme ?cothemetype :properties (?modtype))))
		    
		    ))
		    


;; Conversational responses

(add-state 'greeting
	   (state :action NIL
		  :transitions 
		  (list
		   (transition
		    :pattern ;; hello, good morning
		    '((ONT::SPEECHACT ?!ev ONT::GREET)
		      -greet1>
		      (say :content "Hi"))
		    :trigger t)
		   )))


(add-state 'test
	   (state :action NIL
		  :transitions 
		  (list
		    (transition
		    :pattern ;; what <property> is this?
		    '((ONT::SPEECHACT ?!ev ONT::ask-what-is :what ?!x :aspect ?asp)
		      ((? PP ONT::PRO ONT::PRO-SET) ?!x ?type :proform (? f ont::this ont::these))
		      -describe-what-property>
		      (call (describe-presented-object ?!x :aspect ?asp)))
		    :trigger t)
		    (transition
		    :pattern ;; what <property> is this <object>?"
		    '((ONT::SPEECHACT ?!ev ONT::ask-what-is :what ?!x :aspect ?asp)
		      (ONT::THE ?!x ?!type :proform (? f ont::this ont::these))
		      -describe-what-property2>
		      (call (describe-presented-object ?!x :type ?!type :aspect ?asp)))
		    :trigger t)
		    
		    (transition
		    :pattern ;; what is this
		    '((ONT::SPEECHACT ?!ev ONT::ask-what-is :what ?!x)
		      ((? PP ONT::PRO ONT::PRO-SET) ?!x ?type :proform (? f ont::this ont::these))
		      -ask1>
		      (call (describe-presented-object ?!x)))
		    :trigger t)
		    (transition
		    :pattern ;; what is this object
		    '((ONT::SPEECHACT ?!ev ONT::ask-what-is :what ?!x)
		      (ONT::THE ?!x ONT::REFERENTIAL-SEM :proform (? f ont::this ont::these))
		      -ask2>
		      (call (describe-presented-object ?!x)))
		    :trigger t)
		   
		    (transition
		     :pattern ;; describe this
		     '((ONT::SPEECHACT ?!sa (? satype Ont::fragment ont::request))
		       (ONT::F ?!ev (:* ?xx w::describe) :neutral ?!x)
		       ((? PP ONT::PRO ONT::PRO-SET) ?!x ?type :proform (? f ont::this ont::these))
		      -ask3>
		      (call (describe-presented-object ?!x)))
		     :trigger t)

		  (transition
		    :pattern ;; describe this object
		    '((ONT::F ?!ev (:* ?xx w::describe) :neutral ?!x)
		       (ONT::THE ?!x ONT::REFERENTIAL-SEM :proform (? f ont::this ont::these))
		      -ask4>
		      (call (describe-presented-object ?!x)))
		    :trigger t)
		  
		    (transition
		    :pattern ;; Is this X green?
		    '((ONT::SPEECHACT ?!ev ONT::ask-if :what ?!r)
		      (ONT::F ?!r ONT::HAVE-PROPERTY :property ?!prop :formal ?x)
		      ((? sp ONT::THE ONT::PRO ONT::THE-SET) ?x ?type :proform (? f ont::this ont::these))
		      (ONt::F ?!prop ?proptype)
		      -test-property>
		      (call (test-presented-object ?!x :type ?type :pred ?proptype)))
		    :trigger t)

		    (transition
		    :pattern ;; where are the green blocks?
		    '((ONT::SPEECHACT ?!ev ONT::ask-what-is :what ?!x)
		      (ONT::THE ?!x ONT::LOCATION :suchthat ?!prop)
		      (ONT::F ?!prop ONT::HAVE-PROPERTY :formal ?!obj) ;;:property ?aspect)
		      ((? spec ONT::THE ONT::THE-SET) ?!obj ?type :MOD ?!mod)
		      (ONT::F ?!mod ?property)
		      -locate1>
		      (call (locate-object ?!obj :type ?type :property ?property)))
		    :trigger t)
		  )
		  
	   ))

   
;; Mentions simply pick up noun phases
;; general mentions  - we are just extracting out the definite referring expressions
;;  These rules come last as they shiyld only be used if everything else fails!!!

(add-state 'general-mention
	   (state :action NIL
		  :transitions 
		  (list
		   (transition :pattern 
			 '(((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM)
			    ?!theme ?themetype)
			   -mention>
			   (call (construct-and-send-msg (Mentioned :id ?!theme))))
			 :trigger t)
		   )))

(add-preprocessing 'mention
		   (list
		    (rule :pattern 
			  '(((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM) 
			     ?!theme ?themetype :mod ?!mod :mod1 ?!mod1 :proform -)
			    (ont::F ?!mod ?modtype)
			    (ont::F ?1mod1 ?modtype1)
			    -simple-mention>
			    (Mentioned ?!theme ?themetype :properties (?modtype ?modtype1)))
			  )
		    (rule :pattern 
			  '(((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM) 
			     ?!theme ?themetype  :mod ?!mod :proform -)
			    (ont::F ?!mod (? modtype ont::REFERENTIAL-SEM))
			    -simple-mention1>
			    (Mentioned ?!theme ?themetype :properties (?modtype)))
			  )
		    (rule :pattern
			  '(((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM) 
			     ?!theme ?themetype :assoc-with ?assoc-mod :proform -)
			    -simple-mention2>
			    (Mentioned ?!theme ?themetype :properties ((:assoc-with ?assoc-mod)))
			    ))
		    ))

;; ==========
;;    rules to interpret referring expressions

(setq *term-extraction-rules* '(term-extraction-rules))

(mapcar #'(lambda (x) (im::add-im-rule x 'term-extraction-rules))
	'(
	  (((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!term ?termtype :mod ?!mod)
	   (ont::F ?!mod ?modtype)
	   -one-modifier>
	   (TERM ?!term :class ?termtype :properties (?modtype)))

	  (((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!term ?termtype)
	   -no-modifier>
	   (TERM ?!term :class ?termtype))
	  (((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) 
	    ?!term ?termtype :mod ?!mod :mod1 ?!mod1)
	   (ont::F ?!mod ?modtype)
	   (ont::F ?!mod1 ?modtype1)
	   -two-modifiers>
	   (TERM ?!term :class ?termtype :properties (?modtype ?modtype1)))
	  ((ONT::F ?!ev ?evtype :neutral ?!term :formal ?!mod)
	   ((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET)
	    ?!term ?termtype)
	   (ont::F ?!mod ?modtype :of ?!term)
	   -one-formal-modifier>
	   (TERM ?!term :class ?termtype :properties (?modtype)))
	  
	  (((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!term ?termtype :assoc-with ?!mod)
	   (ont::KIND ?!mod ?modtype)
	   -assoc-modifier>
	   (TERM ?!term :class ?termtype :properties ((:related-to ?modtype)))
	  )
	  (((? reln ONT::A ONT::INDEF-SET ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!term ?termtype :assoc-with ?!assoc :mod ?!mod)
	   (ont::KIND ?!assoc ?assoctype)
	   (ont::F ?!mod ?modtype)
	   -assoc-plus-modifier>
	   (TERM ?!term :class ?termtype :properties ((:related-to ?assoctype) ?modtype)))
	))

;;  domain specific functions

(defun construct-and-send-msg (args channel user uttnum)
  "This processes the LFS to get the right argument structures, and extract the quanitifer and size information"
  (let* ((arg (first args))
	 (id (find-arg-in-act arg :id))
	 (lf (im::find-lf id *most-recent-lfs*))
	 (terms (mapcar #'(lambda (x) (process-term x *most-recent-lfs*))
			(keep-mentioned-terms (find-arg-in-act args :terms) arg)))
	 (class (make-sure-its-a-list (find-arg-in-act arg :class)))
	 (basic-args (append (list* (car arg) :id (replace-with-coref id lf) (cdddr arg)) (list :terms terms)))
	#|| (sizevar (find-arg-in-act lf :size))
	 (size (if sizevar (or (find-arg-in-act (im::find-lf sizevar *most-recent-lfs*) :value)
			       sizevar)))
	 (quan (if (eq (car lf) 'ont::quant)
		   (find-arg-in-act lf :quan)
		   (car lf)))
	 (args-with-size (if size
			     (append basic-args (list :size size))
			     basic-args))
	 (proform  (find-arg-in-act lf :proform))
	 
	 (almostfinal (if quan (append args-with-size (list :quan quan)) args-with-size))
	 (final (if proform (append almostfinal (list :proform proform)) almostfinal)||#
	 )
    (send-msg `(tell :content ,(append basic-args (list :uttnum uttnum) (compute-start-end-of-match))))
    (release-pending-speech-act)
    1
  ))

(defun process-term (term lfs)
  "adds size and quan as defaults"
  (let* ((termlf (im::find-lf (second term) lfs))
	 (sizevar (find-arg-in-act termlf :size))
	 (size (if sizevar (or (find-arg-in-act (im::find-lf sizevar lfs) :value)
			       sizevar)))
	 (quan (if (eq (car termlf) 'ont::quant)
		   (find-arg-in-act termlf :quan)
		   (car termlf)))
	 (term-with-size (if size
			     (append term (list :size size :quan quan))
			     (append term (list :quan quan)))
			     )
	 (proform  (find-arg-in-act termlf :proform)))
    term-with-size))
    

(defun keep-mentioned-terms (terms act)
  "only retain terms that appear in the act"
  (when terms
    (if (member (cadar terms) act)
	(cons (car terms) (keep-mentioned-terms (cdr terms) act))
	(keep-mentioned-terms (cdr terms) act))))

(defun process-properties (props lf)
  ;; eventually we'll construct binary relations, right now we remove them
  (if (consp props)
      (remove-if #'(lambda (x) (or (not (consp x))
				   (intersection x '(:assoc-with ont::assoc-with ont::identity-val w::here w::there W::it))
				   ))
	     (mapcar #'make-sure-its-a-list props))))

(defun make-sure-its-a-list (prop)
  (if (consp prop)
      prop
      (list :* prop prop)))

(defun replace-with-coref (id lf)
  (let* ((coref (find-arg-in-act lf :coref))
	 (proform (find-arg-in-act lf :proform)))
    ;; we don't compute corefs for THIS 
    (if (eq proform 'ont::this)
	id
	(or coref id))))

;;   Code for identifying and describing objects
  
(defun describe-presented-object (args channel user uttnum)
  (let ((id (first args)))
    (send-msg-with-continuation `(request :content ,(append `(identify-object :uttnum ,uttnum :id) args))
				#'(lambda (x) (prepare-description "it is a " x channel)))))

(defun locate-object (args channel user uttnum)
  (let ((id (first args)))
    (send-msg-with-continuation `(request :content ,(append `(positionquery :uttnum ,uttnum :id) args))
				#'(lambda (x) (prepare-description "It is here" x channel)))))

(defun test-presented-object (args channel user uttnum)
  (let ((id (first args)))
    (send-msg-with-continuation `(request :content ,(append `(test-object :uttnum ,uttnum :id)
							   args))
				#'(lambda (x) (prepare-description "The answer is " x channel)))))

(defun prepare-description (prefix desc channel)
  (let* ((class (find-arg-in-act desc :class))
	 (properties (find-arg-in-act desc :properties))
	 (msg (concatenate 'string prefix (if properties (convert-to-words properties) "")
			   (if class (convert-to-words class) "")
			   (if (and (null properties) (null class)) (convert-to-words desc) ""))))
    
  (send-msg `(request :receiver SPEECH-OUT :content (say ,msg :channel ,channel)))))

(defun convert-to-words (x)
  (cond ((eq (car x) :*)
	 (symbol-name (third x)))
	((consp (car x))
	 (concatenate 'string (convert-to-words (car x)) " " (convert-to-words (cdr x))))
	(t "")))
