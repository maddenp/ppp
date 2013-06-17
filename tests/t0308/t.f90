program t1
  implicit none
  character*4::a, open='null'
  open (56, status='new', file='tmpfile', blank=open)
  inquire (56, blank=a)
  print *,a
  close (56,status='delete')
 endprogram t1

