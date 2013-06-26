program t
  implicit none
! open stmt readwrite action, inquire stmt action
  character*11::output
  character*9::a
  open (95, status='new', file='tmpfile', action='readwrite')
  write (95, '(a)') 'hello world'
  backspace (95)
  read (95, '(a)') output
  print *,output
  inquire (95, action=a)
  print *,a
  close (95,status='delete')
endprogram t
