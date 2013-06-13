program t1
  implicit none
  character*10::a
  character*4::inquire='none'
  open (56, status='new', file='tmpfile', delim=inquire)
  inquire (56, delim=a)
  print *,a
   endprogram t1
