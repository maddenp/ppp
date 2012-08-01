program t
  implicit none
  integer::ierr
  character(5)::s
  character(7)::fname='tmpfile'
100 format (i0)
  open(unit=77,file=fname,status='new',iostat=ierr)
  if (ierr.ne.0) stop 'file exists'
  print 100,ierr
  write(77,fmt='(a)',iostat=ierr) 'hello'
  print 100,ierr
  close(77,iostat=ierr)
  print 100,ierr
  open(unit=77,file=fname,status='old',iostat=ierr)
  print 100,ierr
  read(77,fmt='(a)',iostat=ierr) s
  print 100,ierr
  print '(a)',s
  close(77,iostat=ierr)
  print 100,ierr
end program t
