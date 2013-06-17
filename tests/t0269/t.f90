program t
  implicit none
! open stmt replace status
  character*22::a,b
  
  open (unit=95, file='tmpfile', status='new')
  write (95, '(a)') 'this is not the answer'
  rewind (95)
  read (95, '(a)') a
  close (95)
  
  open(unit=85, file='tmpfile', status='replace')
  write (85, '(a)') 'but this is the answer'
  close(85)

  open(85,file='tmpfile',status='old')
  read (85, '(a)') b
  print *,a
  print *,b

  close(85,status='delete')
endprogram t
 
