program t1
  implicit none
! inquire stmt 'exist' specifier
  logical::a=.true.
  inquire (file='tmpfile', exist=a)
  print *,a
  open (95, status='new', file='tmpfile')
  inquire (file='tmpfile', exist=a)
  print *,a
   endprogram t1
