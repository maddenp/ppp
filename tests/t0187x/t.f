      program t
      integer::ierr
      character(5)::a='*****'
      character(5)::b='*****'
 100  format (i0)
      open(unit=77,file='tmpfile',status='new',iostat=ierr)
      write(77,fmt='(a)') 'hello'
      write(77,fmt='(a)') 'world'
      close(77,iostat=ierr)
      open(unit=77,file='tmpfile',status='old',iostat=ierr)
      read(77,fmt='(a)',iostat=ierr) a
      print '(a)',a
      endfile(unit=77,iostat=ierr)
      read(77,fmt='(a)',iostat=ierr) b
      if (ierr.eq.0) then
         stop 'read past eof'
      else
         print '(a)','ok'
      endif
      close(77,status='delete',iostat=ierr)
      end program t
