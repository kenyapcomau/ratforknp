#-h-  cexp                        272  local   12/19/80  15:44:28
 ## common for exptoi
 # put on a file called 'cexp'
 # Used by macro and dc tools
 common/cexp/ top, tokst(MAXSTACK), kindst(MAXSTACK)
 integer top	# evaluation stack pointer
 integer tokst	# eval stack part 1: tokens
 integer kindst # eval stack part 2: kinds of tokens
#-t-  cexp                        272  local   12/19/80  15:44:28
