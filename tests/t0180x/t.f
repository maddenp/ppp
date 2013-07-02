      program t
      integer::ierr,i
      character(5)::s
 100  format (i0)
      open(unit=77,file='infile',status='old',iostat=ierr)
      read(77,fmt='(a)',iostat=ierr) s
      print 100,ierr
      print '(a)',s
      close(77,iostat=ierr)
      end program t
