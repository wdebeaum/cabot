(in-package "IM")

(reset-im-rules 'cabotRules)  ;; this allows you to edit this file and reload it without having to reload the entire system

(mapcar #'(lambda (x) (add-im-rule x 'cabotRules))  ;; sets of rules are tagged so they can be managed independently 
	'(

;;;;;;;;;;;;;;;

#|	  
          ((ONT::F ?ev ?!t)   
           -rule-generic>
           60
           (ONT::EVENT ?ev ?!t
            :rule -rule-generic
            )
           )

          (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev ?!t)   
           -rule-generic2>
           60
           (ONT::TERM ?ev ?!t
            :rule -rule-generic2
            )
           )
|#
	  
;;;;;;;;;;;;;;;

          ; MOVE: move, slide
	  ; PUT: put, place, position, stick
	  ; PUSH/PULL
	  ; TRANSPORT: bring, carry, transport, convey
	  ((?!spec ?ev (:* (? type ONT::MOVE ONT::PUT ONT::PULL ONT::PUSH ONT::TRANSPORT) ?w) :AFFECTED ?!obj :OPERATOR - )
           (?spec2 ?!obj ?t2)
           -rule_move>
           60
           (?!spec ?ev ONT::MOVE
            :rule -rule_move
            :TYPE ?type
	    :LEX ?w
            )
           )	  
	  
	  ((?!spec ?ev (:* (? type ONT::MOVE-UPWARD ONT::PICKUP) ?w) :AFFECTED ?!obj :OPERATOR - )
           (?spec2 ?!obj ?t2)
           -rule_move_up>
           70
           (?!spec ?ev ONT::MOVE
	    :DIRECTION ONT::DIRECTION-UP
            :rule -rule_move_up
            :TYPE ?type
	    :LEX ?w
            )
           )	  

	  ((?!spec ?ev (:* (? type ONT::MOVE-DOWNWARD ) ?w) :AFFECTED ?!obj :OPERATOR - )
           (?spec2 ?!obj ?t2)
           -rule_move_down>
           70
           (?!spec ?ev ONT::MOVE
	    :DIRECTION ONT::DIRECTION-DOWN
            :rule -rule_move_down
            :TYPE ?type
	    :LEX ?w
            )
           )	  

	  ; ACQUIRE: take, grab
	  ; BODY-MANIPULATION: grasp
	  ((?!spec ?ev (:* (? type ONT::ACQUIRE ONT::BODY-MANIPULATION) ?w) :AFFECTED ?!obj :OPERATOR - )
           (?spec2 ?!obj ?t2)
           -rule_acquire>
           60
           (?!spec ?ev ONT::ACQUIRE
            :rule -rule_acquire
            :TYPE ?type
	    :LEX ?w
            )
           )	  

	  )
	)
