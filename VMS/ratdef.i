#-h-  ratdef         8633  local  09/22/80  15:38:26
#================= GENERAL SYMBOL DEFINITIONS =================

# General definitions for software tools
# Should be put on a file named 'ratdef'
# Used by all the tools; read automatically by preprocessor


#   Many of these symbols may change for your particular machine.
#   The values provided are intended as guidelines, and may
#   well serve you adequately, but don't hesitate to change them if
#   necessary.

# In particular, the following might have to change for your system:
#         TERMINAL_IN
#         TERMINAL_OUT
#         MAXLINE
#         FILENAMESIZE
#         DRIVER    and    DRETURN
#         MAXOFILES
#         character

#   Also, watch out for the following definitions, which
#   may conflict with the Fortran operators on your system:
#       AND         OR        NOT


#  Many of the definitions will be used in character variables.
#  They must be defined to be something other than a valid ascii
#  character--such as a number > 255 or a negative number.
#  If you have defined "character" to be "integer", then you may
#  use either a very large number or a small negative number.
#  If you have defined "character" to be something like an 8-bit
#  signed field, you'll need to use negative numbers.
#  Use of a standard integer (whatever is the default size on your
#  machine) is STRONGLY recommended, despite the apparent waste of
#  storage.


# ASCII control character definitions:

define(NUL,8%00)
define(SOH,8%01)
define(STX,8%02)
define(ETX,8%03)
define(EOT,8%04)
define(ENQ,8%05)
define(ACK,8%06)
define(BEL,8%07)
define(BS,8%10)
define(HT,8%11)
define(LF,8%12)
define(VT,8%13)
define(FF,8%14)
define(CR,8%15)
define(SO,8%16)
define(SI,8%17)
define(DLE,8%20)
define(DC1,8%21)
define(DC2,8%22)
define(DC3,8%23)
define(DC4,8%24)
define(NAK,8%25)
define(SYN,8%26)
define(ETB,8%27)
define(CAN,8%30)
define(EM,8%31)
define(SUB,8%32)
define(ESC,8%33)
define(FS,8%34)
define(GS,8%35)
define(RS,8%36)
define(US,8%37)
define(SP,8%40)
define(DEL,8%177)


# Synonyms for important non-printing ASCII characters:

define(BACKSPACE,8%10)
define(BELL,8%07)
define(BLANK,8%40)
define(NEWLINE,8%12)
define(RUBOUT,8%177)
define(TAB,8%11)


# Printable ASCII characters:

define(ACCENT,96)
define(AMPER,38)           # ampersand
define(AMPERSAND,AMPER)
define(AND,AMPER)
define(ATSIGN,64)
define(BACKSLASH,92)
define(BANG,33)            # exclamation mark
define(BAR,124)
define(BIGA,65)
define(BIGB,66)
define(BIGC,67)
define(BIGD,68)
define(BIGE,69)
define(BIGF,70)
define(BIGG,71)
define(BIGH,72)
define(BIGI,73)
define(BIGJ,74)
define(BIGK,75)
define(BIGL,76)
define(BIGM,77)
define(BIGN,78)
define(BIGO,79)
define(BIGP,80)
define(BIGQ,81)
define(BIGR,82)
define(BIGS,83)
define(BIGT,84)
define(BIGU,85)
define(BIGV,86)
define(BIGW,87)
define(BIGX,88)
define(BIGY,89)
define(BIGZ,90)
define(CARET,94)
define(COLON,58)
define(COMMA,44)
define(DASH,45)            #same as MINUS
define(DIG0,48)
define(DIG1,49)
define(DIG2,50)
define(DIG3,51)
define(DIG4,52)
define(DIG5,53)
define(DIG6,54)
define(DIG7,55)
define(DIG8,56)
define(DIG9,57)
define(DOLLAR,36)
define(DQUOTE,34)
define(EQUALS,61)
define(ESCAPE,ATSIGN)      #escape character for ch, find, tr, ed, and sh
define(GREATER,62)
define(LBRACE,123)
define(LBRACK,91)
define(LESS,60)
define(LETA,97)
define(LETB,98)
define(LETC,99)
define(LETD,100)
define(LETE,101)
define(LETF,102)
define(LETG,103)
define(LETH,104)
define(LETI,105)
define(LETJ,106)
define(LETK,107)
define(LETL,108)
define(LETM,109)
define(LETN,110)
define(LETO,111)
define(LETP,112)
define(LETQ,113)
define(LETR,114)
define(LETS,115)
define(LETT,116)
define(LETU,117)
define(LETV,118)
define(LETW,119)
define(LETX,120)
define(LETY,121)
define(LETZ,122)
define(LPAREN,40)
define(MINUS,45)
define(NOT,BANG)  # used in pattern matching; choose ~, ^, or !
define(OR,BAR)
define(PERCENT,37)
define(PERIOD,46)
define(PLUS,43)
define(QMARK,63)
define(RBRACE,125)
define(RBRACK,93)
define(RPAREN,41)
define(SEMICOL,59)
define(SHARP,35)
define(SLASH,47)
define(SQUOTE,39)
define(STAR,42)
define(TAB,9)
define(TILDE,126)
define(UNDERLINE,95)


# Ratfor language extensions:

define(iand,($1 & $2))
define(andif,if)
define(ARB,*)
define(character,byte)  # define character data type
define(DS_DECL,integer $1($2);common/cdsmem/$1)
define(elif,else if)
define(filedes,integer)    # file descriptor/designator data type
define(FILEDES,filedes)
define(IS_DIGIT,(DIG0<=$1&$1<=DIG9))   # valid only for ASCII!
define(IS_LETTER,(IS_UPPER($1)|IS_LOWER($1)))
define(IS_LOWER,(LETA<=$1&$1<=LETZ))
define(IS_UPPER,(BIGA<=$1&$1<=BIGZ))
define(long_real,double precision)
define(max,max0)
define(MAX,max0)
define(min,min0)
define(MIN,min0)
define(pointer,integer)
define(POINTER,integer)



# Input/output modes:

define(APPEND,4)
define(READ,1)
define(READWRITE,3)
define(WRITE,2)


# Standard input/output ports:

define(ERROUT,1)           # standard error file
define(STDERR,ERROUT)
define(STDIN,2)            # standard input file
define(STDOUT,3)           # standard output file
define(STDIN1,4)
define(STDIN2,5)
define(STDIN3,6)


# TERMINAL_IN and TERMINAL_OUT are the names of the I/O channels
# from and to the user's terminal, respectively.  It's highly likely
# there is no such thing on your system; in this case, simply invent
# a name that is not likely to conflict with any file name.
# For example, the VAX/VMS version of the tools uses "TT:", the RSX/11M
# version uses "TT0:", the DEC 10 version uses "tty:", and the Prime
# version uses "/dev/tty".
# Note that you must make the 'open' primitive recognize this name
# and provide access to the terminal accordingly.

define(TERMINAL_IN,"TT:")
define(TERMINAL_OUT,"TT:")


# Manifest constants included for readability and modifiability:

define(ALPHA,-9)
define(ASCII,12)             # flag for ascii character file
define(BEGINNING_OF_FILE,-2) # flag to seek for positioning at
                             # the beginning of a file
define(BINARY,60)            # flag for indicating binary file
define(DIGIT,DIG0)
define(END_OF_FILE,-1)       # flag to seek for positioning at
                             # end of file
define(EOF,-1)
define(EOS,0)
define(ERR,-3)
define(HUGE,30000)           # some arbitrarily large number
define(LAMBDA,0)             # end of list marker
define(LETTER,LETA)
define(LOCAL,6)              # flag for local-type character file
define(NO,0)
define(NOERR,0)              # flag for successful completion
define(OK,-2)                # success flag
define(YES,1)


# Size limiting definitions for important objects:

define(FILENAMESIZE,30)    # max characters in file name
                           # (including EOS)
define(MAXARG,128)         # max size of command line argument
define(MAXCARD,255)        # "card" size
define(MAXCHARS,20)        # max nbr of chars when converting
                           # from integers to characters
                           # (used by putint, outnum, etc.)
define(MAXLINE,256)        # normal size of line buffers;
                           # must be at least 1 more than MAXCARD
define(MAXNAME,FILENAMESIZE)  # max size of file name
define(MAXOFILES,10)       # max nbr opened files allowed at a time
define(MAXPAT,256)         # max size of encoded patterns
                           # (used in string matching)
define(NCHARS,33)          # number of special characters


# Machine-dependent parameters:

define(BITS_PER_CHAR,8)
define(BITS_PER_WORD,32)
define(CHARS_PER_WORD,4)
define(MAX_INTEGER,'7FFFFFFF'X)
define(MIN_INTEGER,'80000000'X)
define(MAX_REAL_EXP,38)
define(MIN_REAL_EXP,-38)
define(REAL_PRECISION,6)


# DRIVER is defined as those things you need to do to start a Software
# Tools program running.  The following is a common approach, but you
# may have to change it (for example, by adding a "program" card).
# Many machines will require no special driver procedure other than
# the call to 'initst'.

define(DRIVER,
   call initst
   ifelse($1,,   call main,   call $1)
   call endst
   end
   ifelse($1,,   subroutine main,   subroutine $1)
   )


# DRETURN is used to finish up a Software Tools program:

define(DRETURN,return)   # (returning from subroutine defined in DRIVER)


# Definitions for 'spawn' primitive (if implemented):

define(WAIT,LETW)              # wait for subprocess to complete
define(NOWAIT,LETN)            # control returns as soon as
                               # subprocess starts
define(BACKGR,LETB)            # spawning a background process


# It may be necessary to add special definitions; for example
# names of important directories, substitute routine names for
# Software Tools primitives that conflict with local subprograms,
# etc.
#-t-  ratdef         8633  local  09/22/80  15:38:26
