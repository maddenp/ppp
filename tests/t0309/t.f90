program t1
  implicit none
  character*4::a, endprogram='zero'
  open (56, status='new', file='tmpfile', blank=endprogram)
  inquire (56, blank=a)
  print *,a
  close (56,status='delete')
 endprogram t1

