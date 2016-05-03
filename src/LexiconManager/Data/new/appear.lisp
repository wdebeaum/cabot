;;;;
;;;; W::appear
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
(W::appear
   (wordfeats (W::morph (:forms (-vb) :nom W::appearance)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("appear-48.1.1") :wn ("appear%2:30:00" "appear%2:30:01"))
     (LF-PARENT ONT::appear)
     (TEMPL affected-templ)
     ;;(PREFERENCE 0.96)
     )
    ((meta-data :origin calo :entry-date 20040916 :change-date nil :comments caloy2)
      (LF-PARENT ONT::APPEARS-TO-HAVE-PROPERTY)
     (example "he appears happy") 
     (TEMPL neutral-theme-complex-subjcontrol-templ)
     )
    ((EXAMPLE "It appears that he is happy")
     (meta-data :origin calo :entry-date 20040916 :change-date nil :comments caloy2)
     (LF-PARENT ONT::appears-to-have-property)
     (SEM (F::Aspect F::stage-level))
     (TEMPL EXPLETIVE-THEME-TEMPL (xp1 (% W::NP (W::lex W::it))) (xp2 (% W::cp (W::ctype W::s-finite))))
     )
    ((EXAMPLE "there appears to be a truck on the corner")
     (LF-PARENT ONT::possibly-true)
     (SEM (F::Aspect F::stage-level))
     (TEMPL EXPLETIVE-THEME-TEMPL (xp1 (% W::NP (W::lex W::there))) (xp2 (% W::cp (W::ctype W::s-to))))
     )
    ((LF-PARENT ONT::APPEARS-TO-HAVE-PROPERTY)
     (meta-data :origin calo :entry-date 20040916 :change-date nil :comments caloy2)
     (example "he appears to be happy")
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-theme-subjcontrol-templ (xp (% W::cp (W::ctype W::s-to))))
     )
    )
   )
))


