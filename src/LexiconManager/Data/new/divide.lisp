;;;;
;;;; W::divide
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::divide
   (wordfeats (W::morph (:forms (-vb) :nom w::division)))
   (SENSES
     ((meta-data :origin monroe :entry-date 20031219 :change-date 20090522 :comments s11 :vn ("separate-23.1-1"))
      (LF-PARENT ont::separation)
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      (templ agent-affected-result-templ (xp (% W::PP (W::ptype W::into))))
      (example "divide the crews into/in teams")
      )
     ((meta-data :origin ptb :entry-date 20100603 :change-date 20090522 :comments s11 :vn ("separate-23.1-1"))
      (LF-PARENT ont::separation)
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      (templ agent-affected-xp-templ)
      (example "critical opinion is divided")
      )
     ((meta-data :origin beetle :entry-date 20080716 :change-date nil :comments nil :vn ("separate-23.1-2"))
      (LF-PARENT ont::separation) ; TODO change to ONT::spatial-divide
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL agent-affected-SOURCE-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? t W::from)))))
      (EXAMPLE "the divider divides the room")
      )
   
     ((lf-parent ont::calc-divide)
      (example "divide by 5")
      (templ agent-theme-xp-templ (xp (% w::pp (w::ptype w::by))))
      (meta-data :origin lam :entry-date 20050420 :change-date 20090522 :comments lam-initial)
      (preference .98) ; prefer separation sense 
      )
     ((lf-parent ONT::calc-divide)
      (example "divide 7 into 21")
      (templ agent-theme-theme-templ)
      (meta-data :origin "wordnet-3.0" :entry-date 20090522 :change-date nil :comments nil)
      (preference .98) ; prefer separation sense 
      )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::divide w::up)
   (SENSES
     ((meta-data :origin monroe :entry-date 20031219 :change-date nil :comments s11)
      (LF-PARENT ont::separation)
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      (example "divide up the crews")
     )
    )
   )
))

