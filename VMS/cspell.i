#-h-  cspell                      483  local   12/05/80  17:09:35
 common / cspell / nlines, dunit, freep, freec, strptr(MAX_DIR_ENTRIES),
		   linptr(2, MAX_DIR_ENTRIES), charay(MAX_CHARS)

 integer nlines		# number of entries read from index
 integer dunit		# rat4 unit for dictionary file
 integer freep		# next free loc in strptr
 integer freec		# next free loc in charay
 integer strptr		# index into charay for the key for the n'th entry
 integer linptr		# values to load into seek to locate record
 character charay	# key strings stored here
#-t-  cspell                      483  local   12/05/80  17:09:35
