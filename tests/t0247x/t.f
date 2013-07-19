      program t
      integer::i
      character(5)::s
      open(unit=77,file='tmpfile',status='new',form='unformatted')
      write(77) 77
      rewind(77)
      read(77) i
      print '(i0)',i
      close (77,status='delete')
      end program t
