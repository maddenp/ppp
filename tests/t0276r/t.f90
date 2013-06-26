program t
  implicit none
! open stmt rewind postion
  character*8::output
  open (95, status='new', file='tmpfile')
  write (95, '(a)') 'record a'
  write (95, '(a)') 'record b'
  open (95, status='old', file='tmpfile', position='rewind')
  read (95, '(a)') output
  print *,output
  close (95,status='delete')
endprogram t
