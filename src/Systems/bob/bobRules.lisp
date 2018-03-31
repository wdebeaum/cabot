(in-package "IM")

#|
(setq *extractor-oblig-features* '(:tense :negation
; :modality
; :degree :frequency 
:progr :perf :passive 
; :coref 
:while :until :purpose 
;:drum    ; we don't automatically pass :drum on because we are constructing logical sequences explicitly in, e.g., -explicit-ref-seq3 (Drumterms)
:ASSOC-WITH  ; for PTM arguments and sites (whcih are chained by :ASSOC-WITH and :MOD)
:MOD
:METHOD ; by-means-of
:REASON ; because
:MANNER ; as a result of

;;;;;; commented this out just for BOB (Let's build a model of the KRAS neighborhood.)
;:OF     ; as a result of
;;;;;;

;:LOCATION ; molecular-site
:PARENTHETICAL ; in-event (NRAS, bound to GTP, binds BRAF.), also mutation, e.g., -simple-ref-mutation2
:LOC       ; Ras in the nucleus/cell line
:LOCATION  ; Ras in the nucleus/cell line
:CONDITION ; if

;;;;; for BOB
:suchthat ; What proteins might lead to the development of pancreatic cancer?
))
|#

; BOB-specific roles
#|
(setq *roles-to-emit* (append *roles-to-emit* '(			    
;    :DISEASE
    :SUCHTHAT
    :TOPIC
    :EXT
    :OPERATOR
    :SEQ
    :CONDITION  ; if (but then need :GROUND too because :CONDITION only contains the linking POS-CONDITION)
    ))
)
|#

; return all roles
;(setq *roles-to-emit* nil)			    


;(setq *extraction-rules* '(bobRules))

;(reset-im-rules 'bobRules)  ;; this allows you to edit this file and reload it without having to reload the entire system

; put this in with the drum_ev rules
(mapcar #'(lambda (x) (add-im-rule x 'drum)) ;'bobRules))  ;; sets of rules are tagged so they can be managed independently 
	'(

	  ; X activates Y, Y activates Z, Z activates W
	  ; this rule is to rename :SEQUENCE to :SEQ so we don't have to extract all :SEQUENCE slots
          ((ONT::F ?ev
;	    ONT::S-CONJOINED :OPERATOR ?!op :SEQUENCE ?!seq :DRUM ?code)
	    ?!t :OPERATOR ?!op :SEQUENCE ?!seq :DRUM ?code)   ; not S-CONJOINED anymore.  It's now the unification of the types of the sequence members
           -rule-seq>
           60
;           (ONT::F ?ev ONT::S-CONJOINED
           (ONT::F ?ev ONT::X-CONJOINED
            :rule -rule-seq
	    :OPERATOR ?!op
	    :SEQ ?!seq
            :DRUM ?code
            )
           )

#|	  
	  ; find 
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::DETERMINE) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code)
           (?reln1 ?!ag  (? t1 ONT::PERSON))
           -rule1>
           60
           (ONT::EVENT ?ev ?type
            :rule -rule1
	    :AGENT -
            :NEUTRAL ?!obj
            :DRUM ?code
            )
           )
|#

	  ; use (arbitrary AFFECTED as long as the AGENT is a person)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::USE) ?!w) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code)
           (?reln1 ?!ag  (? t1 ONT::PERSON))
           -rule1b>
           60
           (ONT::EVENT ?ev ?type
            :rule -rule1b
	    ;:AGENT -
	    :AGENT ?!ag
            :AFFECTED ?!obj
            :DRUM ?code
            )
           )
	  
	  ; build (arbitrary AFFECTED-RESULT as long as the AGENT is a person)
	  ; Note: CREATE is used for ACTIVATE too, but this rule takes precedence because it is at an earlier extraction phase (and it also has a higher priority)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CREATE) ?!w) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code)
           (?reln1 ?!ag  (? t1 ONT::PERSON))
           -rule1c>
           60
           (ONT::EVENT ?ev ?type
            :rule -rule1c
	    ;:AGENT -  ; need to zero this out because otherwise it will go through the regular PRODUCE rule with AGENT only (and eliminate the AFFECTED-RESULT there)
	    :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :DRUM ?code
            )
           )

	  
	  ; treatment
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
;            (:* ONT::CONTROL-MANAGE W::TREATMENT)
            (? type ONT::CONTROL-MANAGE ONT::TREATMENT)
	    :AFFECTED ?!obj :DRUM ?code)
           ((? reln2 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::TERM) ?!obj (? t2 ONT::medical-disorders-and-conditions))
           -rule2>
           60
           (ONT::EVENT ?ev ONT::TREATMENT
            :rule -rule2
;	    :AFFECTED -
;            :DISEASE ?!obj
            :AFFECTED ?!obj
            :DRUM ?code
            )
           )


	  #|
	  ; This is not used any more
	  ; pancreatic cancer
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
	    (:* (? t1 ONT::medical-disorders-and-conditions) ?!w) :SEQUENCE - :DRUM ?code :MODS (?!m))
	   (ONT::F ?!m (:* (? t2 ONT::BODY-PART-VAL) ?!w2))
;           (ONT::EVAL (symbolmap (:* ?t1 ?!w) ?!t1_new))
;           (ONT::EVAL (symbolmap (:* ?t1 ?!w2) ?!t1_new))  ; note: ?!w2, not ?!w
	   -rule3>
	   90
	   (ONT::TERM ?!obj ?t1 ; ?!t1_new
	    :name (?!w2 ?!w)
	    :drum ?code
	    :rule -rule3
	    ))

	  ; model
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* ONT::REPRESENTATION W::MODEL) :FIGURE ?!obj :DRUM ?code)
           ((? reln2 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::TERM) ?!obj (? t2 ONT::DISTRICT W::NEIGHBORHOOD) :ASSOC-WITHS (?!v3))
	   ((? spec3 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::TERM) ?!v3 ?t3)  ; adding the specifiers (in addition to ONT::TERM) would match "model of a staircase"
           -rule2b>
           60
           (ONT::TERM ?ev ONT::MODEL
            :rule -rule2b
            :TOPIC ?!v3
;	    :EXT ONT::SMALL
            :DRUM ?code
            )
           )

	  ; forget it
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::FORGET ONT::CANCEL ONT::RESTART) ?!w))  
           -rule-rollback>
           60
           (ONT::EVENT ?ev ONT::ROLLBACK   ; the arguments (AGENT etc) are automatically passed on
            :rule -rule-rollback
            )
           )
|#

	  
#|
; now we have the full set of DRUMRules

          ;; growth
	  ;; added  ONT::medical-disorders-and-conditions to ?!obj
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::INCREASE ONT::ADD-INCLUDE ONT::ACQUIRE ONT::FILLING ONT::GROW ONT::PROGRESS ONT::DECREASE ONT::PARTS-REMOVED ONT::REMOVE-PARTS ) ?!w) :AFFECTED ?!obj :DRUM ?code )
;           ((? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET TERM) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           ((? reln2 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::TERM) ?!obj (? t2 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE ONT::medical-disorders-and-conditions))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_AGENT_AFFECTED))
           -rule20_AGENT_AFFECTED-AFFECTED>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_AGENT_AFFECTED-AFFECTED
;            :AGENT ?!ag
            :AFFECTED ?!obj
            :DRUM ?code
            )
           )

          ;; target
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::DIRECT-AT ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code )
           ((? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::TERM) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           ((? reln2 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::TERM) ?!obj (? t2 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_NEUTRAL_NEUTRAL1))
           -rule10_NEUTRAL_NEUTRAL1>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_NEUTRAL_NEUTRAL1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :DRUM ?code
            )
           )

          ;; activate
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::START ONT::START-OBJECT ONT::CREATE ONT::CAUSE-MAKE ONT::CAUSE-PRODUCE-REPRODUCE ) ?!w) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code )
           ((? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::TERM) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           ((? reln2 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::TERM) ?!obj (? t2 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40a_AGENT_AFFECTED))
           -rule40a_AGENT_AFFECTED>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40a_AGENT_AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :DRUM ?code
            )
           )
 
          ;; activate
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::START ONT::START-OBJECT ONT::CREATE ONT::CAUSE-MAKE ONT::CAUSE-PRODUCE-REPRODUCE ) ?!w) :AFFECTED ?!obj :DRUM ?code )
;           ((? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::TERM) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           ((? reln2 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::TERM) ?!obj (? t2 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40a_AGENT_AFFECTED))
           -rule40a_AGENT_AFFECTED-AFFECTED>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40a_AGENT_AFFECTED-AFFECTED
;            :AGENT ?!ag
            :AFFECTED ?!obj
            :DRUM ?code
            )
           )

          ;; inactivate
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::STOP ONT::EXTINGUISH ONT::CONSUME ONT::TAKE-IN ONT::DETERIORATE ONT::DESTROY ) ?!w) :AFFECTED ?!obj :DRUM ?code )
;           ((? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::TERM) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           ((? reln2 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::TERM) ?!obj (? t2 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_AGENT_AFFECTED))
           -rule40_AGENT_AFFECTED>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_AGENT_AFFECTED
;            :AGENT ?!ag
            :AFFECTED ?!obj
            :DRUM ?code
            )
           )

|#

	  
;;;;;;;;;;;;;;;
	  
	  )
	)
