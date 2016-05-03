;;;;
;;;; :::take
;;;;

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  ((W::take (W::off))
   (wordfeats (W::morph (:forms (-vb) :past W::took :pastpart W::taken :ing W::taking)))
   (SENSES
    ((LF-PARENT ONT::cause-off)
     (example "take off the coat/take the coat off")
     (SEM (F::Aspect F::bounded) (F::time-span F::extended) (F::cause F::agentive))
     (TEMPL AGENT-AFFECTED-XP-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  ((W::take (W::out))
   (wordfeats (W::morph (:forms (-vb) :past W::took :pastpart W::taken :ing W::taking)))
   (SENSES
    ((EXAMPLE "take out those lights")
     (LF-PARENT ONT::cause-out-of)
     (SEM (F::Aspect F::bounded) (F::time-span F::atomic) (F::cause F::agentive))
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  ((W::take (W::in))
   (wordfeats (W::morph (:forms (-vb) :past W::took :pastpart W::taken :ing W::taking)))
   (SENSES
    ((EXAMPLE "take in the garbage")
     (LF-PARENT ONT::take-in)
     (SEM (F::Aspect F::bounded) (F::time-span F::atomic) (F::cause F::agentive))
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :tags (:base500)
 :words (
  (W::take
   (wordfeats (W::morph (:forms (-vb) :past W::took :pastpart W::taken :ing W::taking)))
   (SENSES
    ((EXAMPLE "He took the aspirin")
     (LF-PARENT ONT::consume)
     (SEM (F::cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (templ agent-affected-xp-templ)
     )
    ;;;; Take a path, e.g., The truck takes the route to Avon
    ((LF-PARENT ONT::MOVE-by-means)
     (SEM (F::Aspect F::bounded) (F::time-span F::extended))
     (TEMPL agent-theme-xp-TEMPL)
     )
    ((LF-PARENT ONT::TAKE-TIME)
     (example "the plan took [him] 5 hours")
     (TEMPL neutral-duration-templ)
     )
    ;;;; it takes 5 hours to complete the plan
    ((LF-PARENT ONT::TAKE-TIME)
     (example "it takes 5 hours to  complete the plan")
     (TEMPL THEME-DURATION-EXPLETIVE-TEMPL)
     (SEM (F::Aspect F::stage-level))
     )
    ((LF-PARENT ONT::TAKE-TIME)
     (example "he took 5 hour to work") 
     (TEMPL neutral-DURATION-COMPLEX-SUBJCONTROL-TEMPL)
     )
   
   ;;;; take notes, pictures
    ((LF-PARENT ONT::create)
     (SEM (F::Cause F::agentive) (F::Aspect F::unbounded) (F::time-span F::extended))
     (TEMPL agent-affected-create-templ)
     )

    ;;;; take a city the preference is low, so that motion senses,
    ;;;; which are more salient for mobile objects, come up on top
    ((LF-PARENT ONT::appropriate)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::time-span F::atomic))
     (PREFERENCE 0.96)
     )
    ;; take the box
    ((LF-PARENT ONT::ACQUIRE)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (preference 1.0) ;; the default 
     (template AGENT-AFFECTED-XP-TEMP)
     )
    ((LF-PARENT ONT::is-compatible-with)
     (SEM (F::Time-span F::extended) (f::trajectory -))
     (example "that projector takes european voltage")
     (templ neutral-neutral-templ)
     (meta-data :origin calo :entry-date 20050308 :change-date nil :comments projector-corpus)
     )
    ((LF-PARENT ONT::take)
     (example "take action" "take a shower")
     (meta-data :origin asma :entry-date 20111005)
     (templ agent-effect-xp-templ)
     (SEM (F::Aspect F::dynamic) (F::Time-span F::extended))
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  ((W::take W::care)
   (wordfeats (W::morph (:forms (-vb) :past W::took :pastpart W::taken)))
   (SENSES
    (;;(LF-PARENT ONT::managing)
     (lf-parent  ont::manage) ;; 20120521 GUM change new parent 
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "he took care of the problem")
     (TEMPL agent-AFFECTED-XP-TEMPL (xp (% W::PP (W::ptype W::of))))
     )    
    )
   )
))

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  ((W::take (w::away))
   (wordfeats (W::morph (:forms (-vb) :past W::took :pastpart W::taken :ing W::taking)))
   (SENSES
    (
     (meta-data :origin particle-verbs :entry-date 20100201)
     (LF-PARENT ONT::cause-come-from)
     (example "take away the cargo" "take it away from the truck")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-xp-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
 ((W::take (w::over))
  (wordfeats (W::morph (:forms (-vb) :past W::took :pastpart W::taken :ing W::taking :nom (w::take w::over))))
   (SENSES
    (
     (meta-data :origin particle-verbs :entry-date 20100201)
     (LF-PARENT ONT::appropriate)
     (example "take the project over" "take over the project")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-xp-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
   ((W::take (w::up))
    (wordfeats (W::morph (:forms (-vb) :past W::took :pastpart W::taken :ing W::taking)))
   (SENSES
    ((meta-data :origin particle-verbs :entry-date 20100201)
     (LF-PARENT ONT::appropriate)
     (example "take the slack up" "take up the slack")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-TEMPL )
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
   ((W::take (w::on))
     (wordfeats (W::morph (:forms (-vb) :past W::took :pastpart W::taken :ing W::taking)))
   (SENSES
    ((meta-data :origin particle-verbs :entry-date 20100201)
     ;;(LF-PARENT ONT::accept)
     ;(lf-parent ont::take-on) ;; 20120524 GUM change new type
     (LF-PARENT ONT::APPROPRIATE)
     (example "take the project on" "take on the project")
     (SEM (F::Aspect F::bounded) )
     (TEMPL AGENT-THEME-XP-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  ((W::take w::a w::chance)
   (wordfeats (W::morph (:forms (-vb) :past W::took :pastpart W::taken :ing W::taking)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("rely-70"))
     (LF-PARENT ont::rely)
     (TEMPL agent-neutral-xp-templ (xp (% w::pp (w::ptype w::on)))) ; like rely,depend,count
     )
    )
   )
))

