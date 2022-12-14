#-h-  csort                      1022  local   12/24/80  14:44:52
 # csort common block - holds information about sort flags
 # put on a file called 'csort'
 # used only by the sorter

 common / csort / linptr(MAXPTR),
		  blanks, dict, fold, noprt, merg, revers, subf, cofset,
		  ifout, ofile(FILENAMESIZE),
		  linbuf(MAXTEXT)

 integer linptr		# pointers to beginning of line in linbuf
 integer blanks		# whether to skip leading blanks in compar; init=NO
 integer dict		# whether to sort in dictionary order     ; init=NO
 integer fold		# whether to fold all characters to lcase ; init=NO
 integer noprt		# whether to ignore non-printing characs  ; init=NO
 integer merg		# whether is a merge only		  ; init=NO
 integer revers		# whether to reverse comparisons	  ; init=NO
 integer subf		# whether sort is on a subfield		  ; init=NO
 integer cofset		# starting column of subfield		  ; init=0
 integer ifout		# if output file specified in command line; init=NO
 character ofile	# file name of +ooutfile specified	  ; init=EOS
 character linbuf	# buffer to hold lines for internal sort
#-t-  csort                      1022  local   12/24/80  14:44:52
