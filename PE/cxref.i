# common block for xref tool
#  put on a file called "cxref"
#  (Used only by 'xref')

common /cxref/ buf(MAXBUF), nextbf
integer buf	# holds trees and linked lists
integer nextbf	# next free element in buf, init = 1
