integer fhash1(MAXLINES),fhash2(MAXLINES)
integer fval1(MAXDIFF),fval2(MAXDIFF),fser1(MAXDIFF),fser2(MAXDIFF)
integer len1,len2,slen1,slen2,pref,suff,clen
integer linect,lastc
char inline
# now the overlaid stuff
integer class(MAXLINES),member(MAXLINES),jvec(MAXVEC)
integer xpart(MAXDIFF),ypart(MAXDIFF),pred(MAXDIFF),klist(MAXDIFF)
equivalence (class(1),fhash1(1)),(member(1),fhash2(1)),(jvec(1),fhash1(1))
equivalence (xpart(1),fval1(1)),(ypart(1),fval2(1)),(pred(1),fser1(1)),
	(klist(1),fser2(1))
# all in common
common /cfile/fhash1,fhash2,fval1,fval2,fser1,fser2,len1,len2,slen1,slen2,
	pref,suff,clen
common /cmisc/inline(INSIZE),linect(FILEB),lastc
