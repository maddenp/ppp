program t1
  implicit none
  character*10::a
  open (56, status='new', file='tmpfile', delim='apostrophe')
  inquire (56, delim=a)
  print *,a
  close (56,status='delete')
   endprogram t1
