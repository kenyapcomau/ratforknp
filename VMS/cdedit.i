#-h-  cdedit                      672  local   12/24/80  14:07:07
 ## common block for ded tool
 #  put on a file called 'cdedit'
 #  (used only by dedit)

 common /cdedit/ cmdbuf(MAXCMD), lastcm,
  nrecs, rec1, rec2, pat(MAXPAT), prevc, cflag
   integer cmdbuf       # buf for commands
   integer lastcm       # next available integer in cmdbuf
   integer nrecs	# number of record numbers
   integer rec1 	# record number 1
   integer rec2 	# record number 2
   integer prevc	# index of previous command
   integer cflag	# character set for input text
#-t-  cdedit                      672  local   12/24/80  14:07:07
