#-h-  libdef         1912  local  09/22/80  15:38:29
 # Symbol definitions for the portable primitives


 # You might need to adjust these for your system:

   define (STDINUNIT,5)    # Unit number for standard input
   define (STDOUTUNIT,6)   # Unit number for standard output
   define (ERROUTUNIT,7)   # Unit number for error output
   define (UNITA,8)        # First available unit (other than
                           # standard ones)
# remaining units assigned by incrementing UNITA by 1 each time
#  define (UNITB,2)        # Next available unit
#  define (UNITC,3)        # Third available unit


 # These definitions shouldn't have to be changed:

   define (DISK,1)         # Flag for disk files (UNITA, UNITB, UNITC)
   define (TERMINAL,0)     # Flag for terminal files (standard input,
                           # output, error output)
   define (MAXARGS,32)     # Max nbr command line arguments allowed
   define (ARGBUFSIZE,MAXLINE) # Size of buffer to hold command line args



# Defines for support library routines

# Defines for memory management routines:
define(DS_MEMEND,1)     # pointer to end of memory
define(DS_AVAIL,2)      # start of available space list
define(DS_CLOSE,8)      # threshhold for close-fitting blocks
define(DS_LINK,1)       # link field of storage block
define(DS_SIZE,0)       # size field of storage block
define(DS_OHEAD,2)      # total words of overhead per block

# Defines for symbol table routines:
define(ST_LINK,0)       # offset of link field in symbol table node
define(ST_DATA,1)       # offset of data field in symbol table node
define(ST_HTABSIZE,43)  # should be a prime number

# Definitions used only for pattern matching
define(AND,AMPER)
define(ANY,QMARK)
define(BOL,PERCENT)
define(CCL,LBRACK)
define(CCLEND,RBRACK)
define(CHAR,LETA)
define(CLOSIZE,4)
define(CLOSURE,STAR)
define(COUNT,1)
define(DASH,MINUS)
define(DITTO,-3)
define(EOL,DOLLAR)
define(NCCL,LETN)
define(NOT,BANG)
define(PREVCL,2)
define(START,3)
define(START_TAG,LBRACE)
define(STOP_TAG,RBRACE)
#-t-  libdef         1912  local  09/22/80  15:38:29
