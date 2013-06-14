program t
  implicit none
  character*10::output
! open stmt direct formatted
  open (88, file='tmpfile', status='new', access='direct', form='formatted', recl=10)
  write (88, '(a9)', rec=2) 'record b'
  
  write (88, '(a)', rec=4) 'record d'

  read (88, '(a6)', rec=2) output
  print*,output
endprogram t
