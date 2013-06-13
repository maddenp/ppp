program t
  implicit none
! open stmt direct unformatted
character*8::output
  open (96, status='new', file='tmpfile', access='direct', form='unformatted', recl=8)
  write (96, rec=6) 'record f'
  write (96, rec=2) 'record b'
  write (96, rec=3) 'record c'
  read (96, rec=2) output
  print*,output
endprogram t
