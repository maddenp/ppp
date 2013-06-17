program t1
  implicit none
  logical::a=.true.
  inquire (file='tmpfile', exist=a)
  print *,a
  open (95, status='new', file='tmpfile')
  inquire (file='tmpfile', exist=a)
  print *,a
  close (95,status='delete')
   endprogram t1
