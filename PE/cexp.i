## common for exptoi
# put on a file called 'cexp'
# Used by macro and dc tools
common/cexp/ top, tokst(MAXSTACK), kindst(MAXSTACK)
integer top	# evaluation stack pointer
integer tokst	# eval stack part 1: tokens
integer kindst	# eval stack part 2: kinds of tokens
