#-h-  cparam                     1148  local   12/01/80  16:02:50
 ## common block holding misc. line info for format tool
 #  put on a file called "cparam"
 #  (used ony by format tool)

 common /cparam/ fill, lsval, inval, rmval, tival, ceval, ulval,
                 boval, cchar, tjust(3), bsval, rjust, cuval,
                 tabs(INSIZE)
   integer fill      # fill if YES; init = YES
   integer lsval   # current line spacing; init = 1
   integer inval   # current indent; >= 0; init = 0
   integer rmval   # current right margin; init = PAGEWIDTH = 60
   integer tival   # current temporary indent; init = 0
   integer ceval   # number of lines to center; init = 0
   integer ulval   # number of lines to underline; init = 0
   integer boval   # number of lines to boldface; init = 0
   character cchar   # line control character; init = PERIOD
   integer tjust   # justification types for heads and foots;
                   # init = LEFT, CENTER, RIGHT
   integer bsval   # number of lines to blank suppress; init=0
   integer rjust   # right justify filled lines if YES; init=YES
   integer cuval   # number lines to continuously underline; init = 0
   integer tabs    # tab stops; init every 8 spaces
#-t-  cparam                     1148  local   12/01/80  16:02:50
