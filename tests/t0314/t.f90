program t1
  implicit none
  character*9::a
  character*3::b,c,d
  open (56, status='new', file='tmpfile', action='write')
  inquire (56, action=a, read=b, write=c, readwrite=d)
  print *,a
  print *,b
  print *,c
  print *,d
  close (56,status='delete')
 endprogram t1
