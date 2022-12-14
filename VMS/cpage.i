#-h-  cpage                      1742  local   12/01/80  16:02:51
 ## common block holding misc. page info for format tool
 #  put on a file called "cpage"
 #   (used only by format tool)

 common /cpage/ curpag,newpag,lineno,plval,m1val,m2val,m3val,m4val,
    bottom, ehead(MAXLINE), ohead(MAXLINE), ehlim(2), ohlim(2),
    efoot(MAXLINE), ofoot(MAXLINE), eflim(2), oflim(2), stopx,
    frstpg, lastpg, print, offset
    integer curpag   # current output page number; init = 0
    integer newpag   # next output page number; init = 1
    integer lineno   # next line to be printed; init = 0
    integer plval   # page length in lines; init = PAGELEN = 66
    integer m1val   # margin before and including header
    integer m2val   # margin after header
    integer m3val   # margin after last text line
    integer m4val   # bottom margin, including footer
    integer bottom   # last live line on page, = plval-m3val-m4val
    character ehead   # top of page title for even pages;init=NEWLINE
    character ohead   # top of page title for odd  pages;init=NEWLINE
    integer ehlim   # left,right margins for even header;init=inval,rmval
    integer ohlim   # left,right margins for odd  header;init=inval,rmval
    character efoot   # bot of page title for even pages;init=NEWLINE
    character ofoot   # bot of page title for odd  pages;init=NEWLINE
    integer eflim   # left,right margins for even footer;init=inval,rmval
    integer oflim   # left,right margins for odd  footer;init=inval,rmval
    integer stopx     # flag for pausing between pages
    integer frstpg    # first page to begin printing with
    integer lastpg    # last page to be printed
    integer print       # flag to indicate whether page should be printed
    integer offset      # number of blanks to offset page by; init = 0
#-t-  cpage                      1742  local   12/01/80  16:02:51
