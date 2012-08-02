program t
  character(len=6)::fname='infile'
  integer::ierr
  logical::p
  p=.false.
! inquire(file=fname,exist=p,iostat=ierr)
  write (*,fmt='(l)',iostat=ierr) .true.
  open(unit=77,file=fname,status='old',iostat=ierr)
  if (ierr.ne.0) then
    stop 'unable to open file: infile'
  endif
  p=.false.
  inquire(unit=77,opened=p,iostat=ierr)
  write (*,fmt='(l)',iostat=ierr) .true.
  close(77)
end program t
