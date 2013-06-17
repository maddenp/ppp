program t
  implicit none
! inquire stmt 'position' specifier (as is)
  character*6::a
  open (56, status='new', file='tmpfile', position='asis')
  inquire (56, position=a)
  print *,a
  close (56,status='delete')
 endprogram t
