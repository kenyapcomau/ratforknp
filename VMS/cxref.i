#-h-  cxref                       236  local   12/24/80  15:47:58
 ## common block for xref tool
 #  put on a file called "cxref"
 #  (Used only by 'xref')

 common /cxref/ buf(MAXBUF), nextbf
   integer buf	        # holds trees and linked lists
   integer nextbf	# next free element in buf, init = 1
#-t-  cxref                       236  local   12/24/80  15:47:58
