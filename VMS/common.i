#-h-  common                     2163  local   12/01/80  15:50:08
# Common blocks used by the Ratfor preprocessor
#     Place on a file called 'common'


 common /cdefio/ bp, buf (BUFSIZE)
    integer bp         # next available character; init = 0
    character buf      # pushed-back characters

 common /cfname/ fcname (MAXNAME)
   character fcname    # text of current function name

 common /cfor/ fordep, forstk (MAXFORSTK)
   integer fordep      # current depth of for statements
   character forstk    # stack of reinit strings

 common /cgoto/ xfer
   integer xfer        # YES if just made transfer, NO otherwise

 common /clabel/ label
   integer label       # next label returned by labgen

 common /cline/ level, linect (NFILES), infile (NFILES),
   fnamp, fnames (MAXFNAMES)
   integer level       # level of file inclusion; init = 1
   integer linect      # line count on input file (level); init = 1
   integer infile      # file number (level); init infile (1) = STDIN
   integer fnamp       # next free slot in fnames; init = 2
   character fnames    # stack of include names; init fnames (1) = EOS

 common /cmacro/ cp, ep, evalst (EVALSIZE), deftbl
    integer cp         # current call stack pointer
    integer ep         # next free position in evalst
    character evalst   # evaluation stack
    pointer deftbl     # symbol table holding macro names

 common /coutln/ outp, outbuf (74)
   integer outp        # last position filled in outbuf; init = 0
   character outbuf    # output lines collected here

 common /csbuf/ sbp, sbuf (SBUFSIZE)
   integer sbp         # next available character position; init = 1
   character sbuf      # saved for data statements

 common /cswtch/ swtop, swlast, swstak (MAXSWITCH)
   integer swtop       # current switch entry; init = 0
   integer swlast      # next available position; init = 1
   integer swstak      # switch information

 common /ckword/ rkwtbl
   pointer rkwtbl      # symbol table containing Ratfor key words

 common /clname/ fkwtbl, namtbl, gentbl
   pointer fkwtbl      # a list of long Fortran keywords
   pointer namtbl      # map of long-form names to short-form names
   pointer gentbl      # list of generated names

 DS_DECL(mem, MEMSIZE)
#-t-  common                     2163  local   12/01/80  15:50:08
