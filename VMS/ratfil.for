	subroutine ratfil(rati,rato,raterr)
	character*(*) rati,rato
	character*80 arglin
	integer raterr,arglen

	raterr=0
	call lib$get_foreign(arglin,,arglen)
	if(arglen.le.0)then
		write(5,500)
500		format(' Input file? ',$)
		read(5,'(a)',end=1000)rati
		write(5,510)
510		format(' Output file? ',$)
		read(5,'(a)',end=1000)rato
	else
		rati=' '
		rato=' '
		i=1
		j=1
		do while(arglin(i:i).ne.' ')
			rati(j:j)=arglin(i:i)
			i=i+1
			j=j+1
		enddo
		i=i+1
		if(i.le.arglen)then
			j=1
			do while(arglin(i:i).ne.' ')
				rato(j:j)=arglin(i:i)
				i=i+1
				j=j+1
			enddo
		endif
	endif
	if(rato.eq.' ')then
		i=index(rati,'.')
		if(i.le.0)then
			rato=rati
		else
			rato=rati(1:i-1)
		endif
	endif
	if(index(rati,'.RAT').le.0)then
		i=index(rati,' ')
		rati=rati(1:i-1)//'.RAT'
	endif
	if(index(rato,'.FOR').le.0)then
		i=index(rato,' ')
		rato=rato(1:i-1)//'.FOR'
	endif
	return
1000	raterr=-1
	return
	end
