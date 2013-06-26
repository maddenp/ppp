program t
  integer::ierr
  character(5)::a='*****'
  character(5)::b='*****'
100 format (i0)
  open(unit=77,file='infile',status='old',iostat=ierr)
  read(77,fmt='(a)',iostat=ierr) a
  print 100,ierr
  print '(a)',a
  backspace 77
  read(77,fmt='(a)',iostat=ierr) b
  print 100,ierr
  print '(a)',b
  close(77,iostat=ierr)
end program t
