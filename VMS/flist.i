#-h-  flist                       282  local   12/24/80  14:44:52
 ## common block used to hold list of files from command line
 #  Put on a file called 'flist'
 # Used by the tools:  sort, format, lpr (VMS version), ls(VMS version)

 #flist - common block

  common /flist/  flevel, ffiles(FILENAMESIZE, FLMAX)
  integer flevel
  character ffiles
#-t-  flist                       282  local   12/24/80  14:44:52
