character lquote,rquote	# left and right quote escape chars
character evalst	# evaluation stack
integer cp		# current call stack pointer
integer ep		# next free position in evalst
common /cmacro/lquote,rquote,cp,ep,evalst(EVALSIZE)
