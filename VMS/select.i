#-h-  select                      650  local   12/24/80  14:44:52
 # select common block - used by sorter
 # put on a file called 'select'
 # used only by the sorter

 common / select / tape, a(TAPENO), d(TAPENO), level, unit(TAPENO), t(TAPENO),
		   file(FILENAMESIZE, TAPENO)

 integer tape	# current tape to write run to; init tape=1
 integer a	# number of runs to date; init a(i)=1 for i=1...TAPENO-1
		#			       a(TAPENO)=0
 integer d	# number of runs to add to tape; init d(i)=1 for i=1...TAPENO-1
		#				      d(TAPENO)=0
 integer level	# Fibonacci level; init level=1
 integer unit	# rat4 unit for tape
 integer t	# array for mapping actual units to virtual units
 character file	# names of temporary files
#-t-  select                      650  local   12/24/80  14:44:52
