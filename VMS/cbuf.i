#-h-  cbuf                        138  local   12/01/80  16:24:31
 # put on "cbuf"
 common /cbuf/  buf(MAXBUF), txtbuf(MAXTXT), lastbf, lastp
 integer buf #buffer for pointers
 character txtbuf #buffer for text
 integer lastbf #last element used in buf
 integer lastp #last element used in txtbuf
#-t-  cbuf                        138  local   12/01/80  16:24:31
