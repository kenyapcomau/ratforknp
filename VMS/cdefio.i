#-h-  cdefio                      279  local   01/06/81  17:02:53
 ## preprocessor common block to hold input characters
 # Put on a file called 'cdefio'
 # Used by ratfor preprocessor, macro, form, and shell tools
 
 common /cdefio/ bp, buf(BUFSIZE)
   integer bp		# next available character; init = 0
   character buf	# pushed-back characters
#-t-  cdefio                      279  local   01/06/81  17:02:53
