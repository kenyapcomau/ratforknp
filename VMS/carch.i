#-h-  carch                       333  local   11/26/80  13:59:23
#-h-  carch          195  local  09/21/80  10:50:05
# carch - common blocks for the archiver

   character fname (FILENAMESIZE, MAXFILES)

   integer fstat (MAXFILES), fcount, errcnt, verbos

  character chead (MAXLINE)      # holds current header
   common /carch/ fname, fstat, fcount, errcnt, verbos,
                  chead
#-t-
#-t-  carch                       333  local   11/26/80  13:59:23
