#-h-  csedit                      672  local   12/24/80  14:07:07
 ## common block for sed tool
 #  put on a file called 'csedit'
 #  (used only by sedit)

 common /csedit/ aq, iq, cmdbuf(MAXCMD), lastcm, buf(MAXBUF), lastbf, 
  nlines, line1, line2, pat(MAXPAT), prevc, nflag
   integer aq		# end of append queue
   integer iq		# end of insert queue
   integer cmdbuf       # buf for commands
   integer lastcm       # next available integer in cmdbuf
   character buf	# buf for text
   integer lastbf	# next available character in buf
   integer nlines	# number of line number expressions
   integer line1	# line number 1 or index to pattern
   integer line2	# line number 2 or index to pattern
   character pat	# current pattern during compilation
   integer prevc	# index of previous command
   integer nflag	# YES to print result of "p" commands only
#-t-  csedit                      672  local   12/24/80  14:07:07
