#-h-  cmac                        181  local   12/01/80  16:02:52
 # common block to hold symbol table for macros
 # put on a file called 'cmac'

 common /cmac/ mactbl
   pointer mactbl    # symbol table containing macros

 DS_DECL (mem, MEMSIZE)
#-t-  cmac                        181  local   12/01/80  16:02:52
