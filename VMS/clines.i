#-h-  clines                      359  local   12/01/80  16:24:32
 # put on "clines"
 common /clines/ line1, line2, nlines, curln, lastln
                                                                                
 integer line1 # first line number
 integer line2 # second line number
 integer nlines # number of line numbers specified
 integer curln  # current line: value of dot
 integer lastln # last lne: value of $
#-t-  clines                      359  local   12/01/80  16:24:32
