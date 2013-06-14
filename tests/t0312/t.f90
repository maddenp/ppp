program t1
  implicit none
  character*6::a
  open (56, status='new', file='tmpfile', position='rewind')
  inquire (56, position=a)
  print *,a
 endprogram t1

