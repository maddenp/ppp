      program t
      implicit none
! open stmt delim specifier 'quote'
      character*19::a
      open (95, file='tmpfile', status='new', delim='quote')
      write (95, *) "he'llo", 'wo"rld'
      close (95)
      open (73, file='tmpfile', status='old', delim='none')
      read (73,'(a)') a
      print*,a
      close (73,status='delete')
      endprogram t
