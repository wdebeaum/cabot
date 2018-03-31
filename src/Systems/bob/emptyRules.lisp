(in-package "IM")

(setq *extraction-rules* '(emptyRules))

(reset-im-rules 'emptyRules)  ;; this allows you to edit this file and reload it without having to reload the entire system

(mapcar #'(lambda (x) (add-im-rule x 'emptyRules))  ;; sets of rules are tagged so they can be managed independently 
	'(

	  
	  )
	)
