#-h-  ctemp                       292  local   12/01/80  16:02:50
 ## common block holding temporary buffers for format tool
 #  put on a file called "ctemp"
 #  (used only by format tool)

 common /ctemp/ tbuf1(MAXLINE), tbuf2(MAXLINE), ttl(MAXLINE)
  character tbuf1   # scratch arrays for use by puttl and tabs
  character tbuf2   #
  character ttl     #
#-t-  ctemp                       292  local   12/01/80  16:02:50
