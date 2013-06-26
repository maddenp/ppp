program t
  implicit none
! open stmt as is position
  character*8::output
  open (95, status='new', file='tmpfile')
  write (95, '(a)') 'record a'
  write (95, '(a)') 'record b'
  open (95, status='old', file='tmpfile', position='asis')
  backspace (95)
  read (95, '(a)') output
  print *,output
  close (95,status='delete')
endprogram t
