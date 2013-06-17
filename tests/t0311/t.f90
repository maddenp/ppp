program t1
  implicit none
! inquire stmt 'position' specifier (append)
  character*6::a
  open (56, status='new', file='tmpfile', position='append')
  inquire (56, position=a)
  print *,a
 endprogram t1

