#-h-  cfiles                      243  local   12/01/80  16:02:51
 ## common block used to hold list of input files
 #  put on a file called 'cfiles'
 #  used by macro, roff

 common /cfiles/ infile(NFILES), level
   integer infile # stack of file descriptors
   integer level # current file is infile(level)
#-t-  cfiles                      243  local   12/01/80  16:02:51
