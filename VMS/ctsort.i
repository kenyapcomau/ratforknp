#-h-  ctsort                      308  local   12/24/80  15:21:57
 ## common block for tsort tool
 #  put on a file caled "ctsort"
 #  used only by tsort

 common /ctsort/ hash(128), nxtsym, nxtfre, buf(MAXBUF)
   integer hash		# hash table headers
   integer nxtsym	# next symbol structure
   integer nxtfre	# next free word at bottom of buf
   integer buf		# free storage
#-t-  ctsort                      308  local   12/24/80  15:21:57
