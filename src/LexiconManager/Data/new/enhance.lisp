;;;;
;;;; W::enhance
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(W::enhance
 (wordfeats (W::morph (:forms (-vb) :nom w::enhancement :agentnom w::enhancer)))
   (SENSES
    ;; handled by the more general frame
    ;((example "It has enhanced software")
     ;(sem (f::aspect f::dynamic))
     ;(meta-data :origin calo :entry-date 20040504 :change-date nil :comments calo-y1variants)
     ;(LF-PARENT ONT::improve)
;     )
    ((example "the light enhanced the color")
     (sem (f::aspect f::dynamic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (meta-data :origin step :entry-date 20080626 :change-date nil :comments nil)
     (LF-PARENT ONT::improve)
     )
    )
   )
))

