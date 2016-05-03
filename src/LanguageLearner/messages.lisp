;;;;
;;;; messages.lisp for Learner Agent
;;;;
;;;;
;;;; Time-stamp: <Thu Aug 16 14:23:35 EDT 2012 jallen>
;;;;

(in-package :llearner)

(in-component :llearner)

(defcomponent-handler
  '(tell &key :content (new-speech-act . *))
  #'(lambda (msg args)
      (new-speech-act args))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (turn-finished . *))
  #'(lambda (msg args)
      (apply #'end-of-turn args))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (demonstrated . *))
  #'(lambda (msg args)
      (interpretation (cons 'demonstrated args)))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (mentioned . *))
  #'(lambda (msg args)
      (interpretation (cons 'mentioned args)))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (described . *))
  #'(lambda (msg args)
      (interpretation (cons 'described args)))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (interpretation-failed . *))
  #'(lambda (msg args)
      (apply #'utt-failure args))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (cps-act-hyps . *))
  #'(lambda (msg args)
      (interp-cps-act-hyps args))
  :subscribe t)

