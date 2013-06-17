program t1
  implicit none
! inquire stmt 'position' specifier (rewind)
  character*6::a
  open (56, status='new', file='tmpfile', position='rewind')
  inquire (56, position=a)
  print *,a
  close (56,status='delete')
 endprogram t1

