#-h-  cout                        468  local   12/01/80  16:02:51
 ## common block holding output lines and info for format tool
 #  put on a file called "cout"
 #  used only by the format tool

 common /cout/ outp, outw, outwds, outbuf(MAXOUT)
   integer outp      # last char position in outbuf; init = 0
   integer outw      # width of text currently in outbuf; init = 0
   integer outwds    # number of words in outbuf; init = 0
   character outbuf  # lines to be filled collect here
                     # word in outbuf; init=0
#-t-  cout                        468  local   12/01/80  16:02:51
