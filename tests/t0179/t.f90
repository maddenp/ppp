program t
  integer::ierr
100 format (i0)
  open(unit=77,file='infile',status='old',iostat=ierr)
  print 100,ierr
  close(77,iostat=ierr)
  print 100,ierr
end program t
